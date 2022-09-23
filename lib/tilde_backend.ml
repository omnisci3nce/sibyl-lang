open Tilde
open Tilde.DataType
open Lexer
open Parser

(* Global context *)
let g_module = Module.create Architecture.X86_64 TargetSystem.Windows
let init_proto = Function.create g_module  I64 0
let init_func = Function.build g_module init_proto "init"
let printf_handle = tb_extern_create g_module "printf"

type register = int

type value_type = VInt | VBool
type value = {
  typ: value_type;
  reg: register
}

let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y.reg)

let type_annot_to_value_t = function
  | Some "bool" -> VBool
  | Some "int"  -> VInt
  | _ -> failwith "unknown type"

let get_tb_type = function
  | VBool -> I8, 1
  | VInt  -> I64, 8

let type_to_tilde_type = function
  | Some t -> get_tb_type t
  | None -> failwith "x64 backend needs type annotations currently"

module Tilde = struct
  let alloc_var fp t env identifier =
    (* Create local *)
    let _, size = get_tb_type t in
    let reg = tb_inst_local fp size size in
    (* Store *)
    Hashtbl.add env identifier { typ = t; reg = reg };
    reg

  let gen_int_const fp x = Inst.i64 fp x
  let gen_bool_const fp x = Inst.u8 fp (if x = true then 1 else 0)
  let gen_add_op fp a b = Inst.add fp a b AssumeNSW
  let gen_sub_op fp a b = Inst.sub fp a b AssumeNSW
  let gen_mul_op fp a b = Inst.mul fp a b AssumeNSW
  let gen_div_op fp a b = Inst.div fp a b AssumeNSW
  let gen_less_than_op fp a b = Inst.less_than fp a b
  let gen_and_op fp a b = Inst.logical_and fp a b
  let gen_or_op fp a b = Inst.logical_or fp a b

  let rec gen_from_expr fp func_env var_env = function
    | IntConst i -> { typ = VInt; reg = gen_int_const fp i }
    | Bool b -> { typ = VBool; reg = gen_bool_const fp b }
    | Grouping e -> gen_from_expr fp func_env var_env e.expr
    | Binary bin -> begin
      let a = gen_from_expr fp func_env var_env bin.left_expr in
      let b = gen_from_expr fp func_env var_env bin.right_expr in
      match bin.operator.token_type with
      | Plus  -> { typ = VInt; reg = (gen_add_op fp a.reg b.reg) }
      | Minus  -> { typ = VInt; reg = (gen_sub_op fp a.reg b.reg) }
      | Star  -> { typ = VInt; reg = (gen_mul_op fp a.reg b.reg) }
      | Slash  -> { typ = VInt; reg = (gen_div_op fp a.reg b.reg) }
      | LessThan  -> { typ = VBool; reg = (gen_less_than_op fp a.reg b.reg) }
      | _ -> failwith "todo"
    end
    | IfElse ie ->
      (* Setup result register *)
      let result = tb_inst_local fp 8 8 in

      (* Evaluate condition *)
      let cond = gen_from_expr fp func_env var_env ie.condition in

      (* Setup labels *)
      let if_true = tb_inst_new_label_id fp
      and if_false = tb_inst_new_label_id fp
      and end_if = tb_inst_new_label_id fp in

      (* Branch *)
      let _ = tb_inst_if fp cond.reg if_true if_false in

      (* Then branch *)
      let _ = tb_inst_label fp if_true in
        (* Evaluate then branch *)
        let then_result = gen_from_expr fp func_env var_env ie.then_branch in
        let _ = Inst.store fp I64 result then_result.reg 8 in
        (* Goto end *)
        let _ = tb_inst_goto fp end_if in

      (* Else branch *)
      let _ = tb_inst_label fp if_false in
        (* Evaluate else branch *)
        let else_result = gen_from_expr fp func_env var_env ie.else_branch in
        let _ = Inst.store fp I64 result else_result.reg 8 in

      (* End IfElse expression *)
      let _ = tb_inst_label fp end_if in
      (* Return register with expression result *)
      { typ = then_result.typ; reg = result }

    | Unary _ -> failwith "todo: Unary"
    | Logical l ->
        let left = gen_from_expr fp func_env var_env l.left_expr
        and right = gen_from_expr fp func_env var_env l.right_expr in begin
        match l.operator.token_type with
          | And -> { typ = VBool; reg = (gen_and_op fp left.reg right.reg) }
          | Or  -> { typ = VBool; reg = (gen_or_op fp left.reg right.reg) }
          | _ -> failwith "todo: other logical operators"
        end
    | Var v ->
                let value = Hashtbl.find var_env v in
                let ttype, size = get_tb_type value.typ in
                let reg = Inst.load fp ttype value.reg size in
                { value with reg = reg} 
    | Call c -> (
        let ident = match c.callee with
          | Var s -> s
          | _ -> failwith "todo" in
        let args = List.map (fun arg -> let e = gen_from_expr fp func_env var_env arg in e.reg) c.arguments in 
        let arr = make_params_array args in
        let function_pointer, rtype = Hashtbl.find func_env ident in
        let result = tb_inst_call fp (get_datatype rtype) function_pointer (List.length c.arguments) (Ctypes.CArray.start arr) in
        { typ = VInt; reg = result }
      )
    | Unit -> failwith "todo: Unit"

  let rec gen_from_stmt fp func_env var_env = function
    | LetDecl ld ->
        (* Allocate variable *)
        let value_type = ld.type_annot |> type_annot_to_value_t in
        let ttype, size =  get_tb_type value_type in
        let name = alloc_var fp value_type var_env ld.identifier in
        (* Evaluate *)
        let value = gen_from_expr fp func_env var_env ld.expr in
        (* Store *)
        let _ = Inst.store fp ttype name value.reg size in
        ()
    | FunctionDecl { name; arity; params; body; return_type_annot } ->
      let rtype, _ = return_type_annot |> type_annot_to_value_t |> get_tb_type in
        let func_proto = Function.create g_module rtype arity in
        List.iteri 
          (fun _ param ->
            let ttype, _ = param.type_annot |> type_annot_to_value_t |> get_tb_type in
            let _ = function_add_param func_proto (get_datatype ttype) in ()
          ) params; 
        let func = Function.build g_module func_proto name  in
        Hashtbl.add func_env name (func, rtype);

        (* Add params into env  *)
        let scoped_env = Hashtbl.create (List.length params) in

        List.iteri (fun i param -> 
          let p_addr = tb_inst_param_addr func i in
          let v = { typ = (type_annot_to_value_t param.type_annot); reg = p_addr } in
          Hashtbl.add scoped_env param.token.lexeme v
        ) params;

        let rec inner env stmts = match stmts with
          | [] -> ()
          | s :: rest -> gen_from_stmt func func_env env s; inner env rest in
        (* Generate function body *)
        inner scoped_env body;

        () 
    | Print e -> (
      match e with
      | Var v ->
        let var = Hashtbl.find var_env v in
        let ttype, size = var.typ |> get_tb_type in
        let format_string = tb_inst_cstring fp "result: %lld\n" in
        let value = Inst.load fp ttype var.reg size in
        let cast_to_i64 = tb_inst_zxt fp value (get_datatype I64) in
        let arr = make_params_array [format_string; cast_to_i64] in
        let _ = tb_inst_ecall fp void_dt printf_handle 2 (Ctypes.CArray.start arr) in
        ()
      | _ -> failwith "todo"
    ) 
    | Expression _ -> failwith "todo"
    | Return ve ->

      let value = gen_from_expr fp func_env var_env ve.value in
      Inst.return fp value.reg;
      let _ = function_compile g_module fp in
      ()
  let codegen (ast: statement list) =
    (* Generate code for each statement *)
    let var_env = Hashtbl.create 10 in
    let func_env : (string, function_t Ctypes.structure Ctypes_static.ptr * type_t) Hashtbl.t = Hashtbl.create 10 in
    List.iter (fun stmt -> gen_from_stmt init_func func_env var_env stmt; ()) ast;
    let return_code = Inst.i64 init_func 0 in
    (* Return exit code from main function *)
    Inst.return init_func return_code;
    (* Compilation of main function and module *)
    let _ = function_compile g_module init_func in
    let _ = module_compile g_module in
    let _ = module_export g_module "./test_x64.obj" true in
    ()
end

open Tilde
open Tilde.DataType
open Lexer
open Parser

(* Global context *)
let g_module = Module.create Architecture.X86_64 TargetSystem.Windows
let init_func = Function.create g_module "init" I64 []
let printf_handle = tb_extern_create g_module "printf"

type register = int

type value_type = VInt | VBool
type value = {
  typ: value_type;
  reg: register
}

let variables = Hashtbl.create 100
let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y.reg)

let current_function_pointer = ref init_func

module Tilde = struct
  let alloc_var identifier =
    (* Create local *)
    let reg = tb_inst_local init_func 8 8 in
    (* Store *)
    Hashtbl.add variables identifier { typ = VInt; reg = reg };
    reg

  let gen_int_const x = Inst.i64 init_func x
  let gen_bool_const x = Inst.u8 init_func (if x = true then 1 else 0)
  let gen_add_op a b = Inst.add init_func a b AssumeNSW
  let gen_sub_op a b = Inst.sub init_func a b AssumeNSW
  let gen_mul_op a b = Inst.mul init_func a b AssumeNSW
  let gen_div_op a b = Inst.div init_func a b AssumeNSW
  let gen_less_than_op a b = Inst.less_than init_func a b
  let gen_and_op a b = Inst.logical_and init_func a b
  let gen_or_op a b = Inst.logical_or init_func a b

  let rec gen_from_expr env = function
    | IntConst i -> { typ = VInt; reg = gen_int_const i }
    | Bool b -> { typ = VBool; reg = gen_bool_const b }
    | Grouping e -> gen_from_expr env e.expr
    | Binary bin -> begin
      let a = gen_from_expr env bin.left_expr in
      let b = gen_from_expr env bin.right_expr in
      match bin.operator.token_type with
      | Plus  -> { typ = VInt; reg = (gen_add_op a.reg b.reg) }
      | Minus  -> { typ = VInt; reg = (gen_sub_op a.reg b.reg) }
      | Star  -> { typ = VInt; reg = (gen_mul_op a.reg b.reg) }
      | Slash  -> { typ = VInt; reg = (gen_div_op a.reg b.reg) }
      | LessThan  -> { typ = VBool; reg = (gen_less_than_op a.reg b.reg) }
      | _ -> failwith "todo"
    end
    | IfElse ie ->
      let fp = init_func in

      (* Setup result register *)
      let result = tb_inst_local fp 8 8 in

      (* Evaluate condition *)
      let cond = gen_from_expr env ie.condition in

      (* Setup labels *)
      let if_true = tb_inst_new_label_id fp
      and if_false = tb_inst_new_label_id fp
      and end_if = tb_inst_new_label_id fp in

      (* Branch *)
      let _ = tb_inst_if fp cond.reg if_true if_false in

      (* Then branch *)
      let _ = tb_inst_label fp if_true in
        (* Evaluate then branch *)
        let then_result = gen_from_expr env ie.then_branch in
        let _ = Inst.store fp I64 result then_result.reg 8 in
        (* Goto end *)
        let _ = tb_inst_goto fp end_if in

      (* Else branch *)
      let _ = tb_inst_label fp if_false in
        (* Evaluate else branch *)
        let else_result = gen_from_expr env ie.else_branch in
        let _ = Inst.store fp I64 result else_result.reg 8 in

      (* End IfElse expression *)
      let _ = tb_inst_label fp end_if in
      (* Return register with expression result *)
      { typ = then_result.typ; reg = result }

    | Unary _ -> failwith "todo: Unary"
    | Logical l ->
        let left = gen_from_expr env l.left_expr
        and right = gen_from_expr env l.right_expr in begin
        match l.operator.token_type with
          | And -> { typ = VBool; reg = (gen_and_op left.reg right.reg) }
          | Or  -> { typ = VBool; reg = (gen_or_op  left.reg right.reg) }
          | _ -> failwith "todo: other logical operators"
        end
    | Var v -> Hashtbl.find env v
    | Call _ -> failwith "todo: Call"
    | Unit -> failwith "todo: Unit"
  let rec gen_from_stmt var_env = function
    | LetDecl ld ->
        (* Allocate variable *)
        let name = alloc_var ld.identifier in
        (* Evaluate *)
        let value = gen_from_expr var_env ld.expr in
        (* Store *)
        let _ = Inst.store init_func I64 name value.reg 8 in
        ()
    | FunctionDecl { name; params; body } ->
        let param_types = [I64] in
        let func = Function.create g_module name I64 param_types in
        current_function_pointer := func;

        (* Add params into env  *)
        let env = Hashtbl.create (List.length params) in
        List.iteri (fun i tok -> Hashtbl.add env tok.lexeme (tb_inst_param_addr func i)) params;

        let rec inner env stmts = match stmts with
          | [] -> ()
          | s :: rest -> gen_from_stmt env s; inner env rest in
        (* Generate function body *)
        inner var_env body;

        () 
    | Print e -> (
      match e with
      | Var v ->
        let format_string = tb_inst_cstring init_func "result: %lld\n" in
        let var = Hashtbl.find variables v in
        let value = Inst.load init_func I64 var.reg 8 in
        let arr = make_params_array [format_string; value] in
        let _ = tb_inst_ecall init_func void_dt printf_handle 2 (Ctypes.CArray.start arr) in
        ()
      | _ -> failwith "todo"
    ) 
    | Expression _ -> failwith "todo"
    | Return ve ->
      let value = gen_from_expr var_env ve.value in
      Inst.return !current_function_pointer value.reg;
      current_function_pointer := init_func;
      ()
  let codegen (ast: statement list) =
    (* Generate code for each statement *)
    List.iter (fun stmt -> gen_from_stmt variables stmt; ()) ast;
    let return_code = Inst.i64 init_func 0 in
    (* Return exit code from main function *)
    Inst.return init_func return_code;
    (* Compilation of main function and module *)
    let _ = function_compile g_module init_func in
    let _ = module_compile g_module in
    let _ = module_export g_module "./test_x64.obj" true in
    print_hashtbl variables;
    ()
end

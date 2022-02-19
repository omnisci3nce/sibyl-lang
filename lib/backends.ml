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

let variables = Hashtbl.create 100
let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y.reg)

let current_function_pointer = ref init_func
let hack = ref init_func

module Tilde = struct
  let alloc_var env identifier =
    (* Create local *)
    let reg = tb_inst_local !current_function_pointer 8 8 in
    (* Store *)
    Hashtbl.add env identifier { typ = VInt; reg = reg };
    reg

  let gen_int_const x = Inst.i64 current_function_pointer x
  let gen_bool_const x = Inst.u8 current_function_pointer (if x = true then 1 else 0)
  let gen_add_op a b = Inst.add current_function_pointer a b AssumeNSW
  let gen_sub_op a b = Inst.sub current_function_pointer a b AssumeNSW
  let gen_mul_op a b = Inst.mul current_function_pointer a b AssumeNSW
  let gen_div_op a b = Inst.div current_function_pointer a b AssumeNSW
  let gen_less_than_op a b = Inst.less_than current_function_pointer a b
  let gen_and_op a b = Inst.logical_and current_function_pointer a b
  let gen_or_op a b = Inst.logical_or current_function_pointer a b

  let rec gen_from_expr env = function
    | IntConst i -> { typ = VInt; reg = gen_int_const i }
    | Bool b -> { typ = VBool; reg = gen_bool_const b }
    | Grouping e -> gen_from_expr env e.expr
    | Binary bin -> begin
      let a = gen_from_expr env bin.left_expr in
      print_endline "HERE5\n";
      let b = gen_from_expr env bin.right_expr in
      print_endline "HERE6\n";
      match bin.operator.token_type with
      | Plus  -> { typ = VInt; reg = (gen_add_op a.reg b.reg) }
      | Minus  -> { typ = VInt; reg = (gen_sub_op a.reg b.reg) }
      | Star  -> { typ = VInt; reg = (gen_mul_op a.reg b.reg) }
      | Slash  -> { typ = VInt; reg = (gen_div_op a.reg b.reg) }
      | LessThan  -> { typ = VBool; reg = (gen_less_than_op a.reg b.reg) }
      | _ -> failwith "todo"
    end
    | IfElse ie ->
      let fp = current_function_pointer in

      (* Setup result register *)
      let result = tb_inst_local !fp 8 8 in

      (* Evaluate condition *)
      let cond = gen_from_expr env ie.condition in

      (* Setup labels *)
      let if_true = tb_inst_new_label_id !fp
      and if_false = tb_inst_new_label_id !fp
      and end_if = tb_inst_new_label_id !fp in

      (* Branch *)
      let _ = tb_inst_if !fp cond.reg if_true if_false in

      (* Then branch *)
      let _ = tb_inst_label !fp if_true in
        (* Evaluate then branch *)
        let then_result = gen_from_expr env ie.then_branch in
        let _ = Inst.store fp I64 result then_result.reg 8 in
        (* Goto end *)
        let _ = tb_inst_goto !fp end_if in

      (* Else branch *)
      let _ = tb_inst_label !fp if_false in
        (* Evaluate else branch *)
        let else_result = gen_from_expr env ie.else_branch in
        let _ = Inst.store fp I64 result else_result.reg 8 in

      (* End IfElse expression *)
      let _ = tb_inst_label !fp end_if in
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
    | Var v -> print_endline "Hello\n"; flush stdout;
                let value = Hashtbl.find env v in
                print_string v; flush stdout; value
    | Call c -> (
        (* let ident = match c.callee with
          | Var s -> s
          | _ -> failwith "cant call with non-var expression yet" in *)
        print_endline "call!\n";
        print_hashtbl env;

        let arg = gen_from_expr env (List.nth c.arguments 0) in 
        let arr = make_params_array [arg.reg] in
        let result = tb_inst_call !hack (get_datatype I64) !hack 1 (Ctypes.CArray.start arr) in
        { typ = VInt; reg = result }
      )
    | Unit -> failwith "todo: Unit"

  let rec gen_from_stmt var_env = function
    | LetDecl ld ->
        print_endline "HERE0\n";
        (* Allocate variable *)
        let name = alloc_var var_env ld.identifier in
        (* Evaluate *)
        print_endline "HERE1\n";
        print_hashtbl var_env;
        print_endline (string_of_expr ld.expr);
        flush stdout;
        let value = gen_from_expr var_env ld.expr in
        (* Store *)
        print_endline "HERE2\n";
        let _ = Inst.store current_function_pointer I64 name value.reg 8 in
        print_endline "HERE3\n";
        ()
    | FunctionDecl { name; params; body } ->
        let func_proto = Function.create g_module I64 1 in
        List.iteri 
          (fun _ _ ->
            let _ = function_add_param func_proto (DataType.get_datatype I64) in ()
          ) params; 
        let func = Function.build g_module func_proto name  in
        current_function_pointer := func;
        hack := func;

        (* Add params into env  *)
        let func_env = Hashtbl.create (List.length params) in

        List.iteri (fun i tok -> 
          print_endline "1 param\n";
          let v = { typ = VInt; reg = tb_inst_param_addr func i } in
          Hashtbl.add func_env tok.lexeme v
        ) params;

        let rec inner env stmts = match stmts with
          | [] -> ()
          | s :: rest -> print_stmt s; gen_from_stmt env s; inner env rest in
        (* Generate function body *)
        inner func_env body;

        () 
    | Print e -> (
      match e with
      | Var v ->
        let format_string = tb_inst_cstring !current_function_pointer "result: %lld\n" in
        Printf.printf "var: %s\n" v;
        let var = Hashtbl.find var_env v in
        let value = Inst.load current_function_pointer I64 var.reg 8 in
        let arr = make_params_array [format_string; value] in
        let _ = tb_inst_ecall !current_function_pointer void_dt printf_handle 2 (Ctypes.CArray.start arr) in
        ()
      | _ -> failwith "todo"
    ) 
    | Expression _ -> failwith "todo"
    | Return ve ->
      let value = gen_from_expr var_env ve.value in
      Inst.return current_function_pointer value.reg;
      let _ = function_compile g_module !current_function_pointer in
      current_function_pointer := init_func;
      ()
  let codegen (ast: statement list) =
    (* Generate code for each statement *)
    List.iter (fun stmt -> gen_from_stmt variables stmt; ()) ast;
    let return_code = Inst.i64 current_function_pointer 0 in
    (* Return exit code from main function *)
    Inst.return current_function_pointer return_code;
    (* Compilation of main function and module *)
    let _ = function_compile g_module init_func in
    let _ = module_compile g_module in
    let _ = module_export g_module "./test_x64.obj" true in
    ()
end

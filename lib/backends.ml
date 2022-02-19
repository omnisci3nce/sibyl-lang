open Parser
open Tilde
open Tilde.DataType

(* Only deal with 64bit ints at the moment *)

(* Global context *)
let g_module = Module.create Architecture.X86_64 TargetSystem.Windows
let init_proto = Function.create g_module I64
let init_func = Function.build g_module init_proto "init"
let printf_handle = tb_extern_create g_module "printf"

let variables = Hashtbl.create 100

module Tilde = struct
  let alloc_var identifier =
    (* Create local *)
    let reg = tb_inst_local init_func 8 8 in
    (* Store *)
    Hashtbl.add variables identifier reg;
    reg

  let gen_int_const x =
    Inst.i64 init_func x
  let gen_add_op a b =
    Inst.add init_func a b AssumeNSW
  let gen_sub_op a b =
    Inst.sub init_func a b AssumeNSW
  let gen_mul_op a b =
    Inst.mul init_func a b AssumeNSW
  let gen_div_op a b =
    Inst.div init_func a b AssumeNSW
  let rec gen_from_expr = function
    | IntConst i -> gen_int_const i
    | Grouping e -> gen_from_expr e.expr
    | Binary b -> begin
      match b.operator.token_type with
      | Plus -> 
        let a = gen_from_expr b.left_expr
        and b = gen_from_expr b.right_expr in
        gen_add_op a b
      | Minus ->
        let a = gen_from_expr b.left_expr
        and b = gen_from_expr b.right_expr in
        gen_sub_op a b
      | Star -> 
        let a = gen_from_expr b.left_expr
        and b = gen_from_expr b.right_expr in
        gen_mul_op a b
      | Slash -> 
        let a = gen_from_expr b.left_expr
        and b = gen_from_expr b.right_expr in
        gen_div_op a b
      | _ -> failwith "todo"
    end
    | _ -> failwith "todo"
  let gen_from_stmt = function
    | LetDecl ld ->
        (* Allocate variable *)
        let name = alloc_var ld.identifier in
        (* Evaluate *)
        let value = gen_from_expr ld.expr in
        (* Store *)
        let _ = Inst.store init_func I64 name value 8 in
        ()
    | FunctionDecl _ -> failwith "todo"
    | Print e -> (
      match e with
      | Var v ->
        let format_string = tb_inst_cstring init_func "sum: %lld\n" in
        let var = Hashtbl.find variables v in
        let value = Inst.load init_func I64 var 8 in
        let arr = make_params_array [format_string; value] in
        let _ = tb_inst_ecall init_func void_dt printf_handle 2 (Ctypes.CArray.start arr) in
        ()
      | _ -> failwith "todo"
    ) 
    | Expression _ -> failwith "todo"
    | Return _ -> failwith "todo"
  let codegen (ast: statement list) =
    (* Generate code for each statement *)
    List.iter (fun stmt -> gen_from_stmt stmt; ()) ast;
    let return_code = Inst.i64 init_func 0 in
    (* Return exit code from main function *)
    Inst.return init_func return_code;
    (* Compilation of main function and module *)
    let _ = function_compile g_module init_func in
    let _ = module_compile g_module in
    let _ = module_export g_module "./test_x64.obj" true in
    Helpers.print_hashtbl variables;
    ()

end

open Parser
open Tilde
open Tilde.DataType

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

  let gen_int_const x = Inst.i64 init_func x
  let gen_add_op a b = Inst.add init_func a b AssumeNSW
  let gen_sub_op a b = Inst.sub init_func a b AssumeNSW
  let gen_mul_op a b = Inst.mul init_func a b AssumeNSW
  let gen_div_op a b = Inst.div init_func a b AssumeNSW
  let gen_less_than_op a b = Inst.less_than init_func a b

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
      | LessThan ->
        let a = gen_from_expr b.left_expr
        and b = gen_from_expr b.right_expr in
        let result = gen_less_than_op a b in
        result
      | _ -> failwith "todo"
    end
    (* 
  TB_Label if_true = tb_inst_new_label_id(func);
	TB_Label if_false = tb_inst_new_label_id(func);

    // if (n < 2)
	tb_inst_if(func, tb_inst_cmp_ilt(func, n, tb_inst_sint(func, TB_TYPE_I32, 2), true), if_true, if_false);
	
	// then
	{
		tb_inst_label(func, if_true);
		tb_inst_ret(func, n);
	}
	
	// else
	{
		tb_inst_label(func, if_false);
		
		TB_Register n_minus_one = tb_inst_sub(func, n, tb_inst_sint(func, TB_TYPE_I32, 1), TB_ASSUME_NUW);
		TB_Register call1 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_one });
		
		TB_Register n_minus_two = tb_inst_sub(func, n, tb_inst_sint(func, TB_TYPE_I32, 2), TB_ASSUME_NUW);
		TB_Register call2 = tb_inst_call(func, TB_TYPE_I32, func, 1, (TB_Register[]) { n_minus_two });
		
		TB_Register sum = tb_inst_add(func, call1, call2, TB_ASSUME_NUW);
		tb_inst_ret(func, sum);
	}
    *)
    | IfElse ie ->
      let fp = init_func in

      (* Setup result register *)
      let result = tb_inst_local fp 8 8 in

      (* Evaluate condition *)
      let cond = gen_from_expr ie.condition in

      (* Setup labels *)
      let if_true = tb_inst_new_label_id fp
      and if_false = tb_inst_new_label_id fp
      and end_if = tb_inst_new_label_id fp in

      (* Branch *)
      let _ = tb_inst_if fp cond if_true if_false in

      (* Then branch *)
      let _ = tb_inst_label fp if_true in
        (* Evaluate then branch *)
        let then_result = gen_from_expr ie.then_branch in
        let _ = Inst.store fp I64 result then_result 8 in
        (* Goto end *)
        let _ = tb_inst_goto fp end_if in

      (* Else branch *)
      let _ = tb_inst_label fp if_false in
        (* Evaluate else branch *)
        let else_result = gen_from_expr ie.else_branch in
        let _ = Inst.store fp I64 result else_result 8 in

      (* End IfElse expression *)
      let _ = tb_inst_label fp end_if in
      (* Return register with expression result *)
      result

    | Bool _ -> failwith "todo: Bool"
    | Unary _ -> failwith "todo: Unary"
    | Logical _ -> failwith "todo: Logical"
    | Var _ -> failwith "todo: Var"
    | Call _ -> failwith "todo: Call"
    | Unit -> failwith "todo: Unit"
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
        let format_string = tb_inst_cstring init_func "result: %lld\n" in
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

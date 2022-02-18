open Parser

(* module Make (CG : CodeGenerator) = struct
  include CG

  let rec gen_from_expr gen expr : (generator * string) = match expr with
    | Grouping e -> gen_from_expr gen e.expr
    | Binary b -> begin
      match b.operator with
      | t when t.token_type = Plus -> begin
        match b.left_expr, b.right_expr with
        (* Num + Num *)
        | IntConst a,  IntConst b ->
          let (name, _) = gen_plus_op (string_of_int a) (string_of_int b) gen in
          gen, name
        | IntConst a, Var b ->
          let temp_name, _ = alloc_temp_var gen in
          let new_gen = gen_copy_ident temp_name (var gen b) gen in
          let (name, _offset) = gen_plus_op (string_of_int a) (var new_gen temp_name) new_gen in
          new_gen, name

        (* Num + Expr *)
        | IntConst a, e ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let (name, _offset) = gen_plus_op (string_of_int a) (var new_gen temp_name) new_gen in
          new_gen, name
        (* Expr + Num *)
        | e, IntConst a ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let (name, _offset) = gen_plus_op (string_of_int a) (var new_gen temp_name) new_gen in
          new_gen, name
        | le, re ->
          let (new_gen, left_temp_name) = gen_from_expr gen le in
          let new_gen, right_temp_name = gen_from_expr new_gen re in
          let name, _ = gen_plus_op left_temp_name right_temp_name new_gen in
          new_gen, name
      end
      | t when t.token_type = Minus -> begin
        match b.left_expr, b.right_expr with
        (* Expr - Num *)
        | e, IntConst a ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let (name, _offset) = gen_sub_op (var new_gen temp_name) (string_of_int a) new_gen in
          new_gen, name
        | _ -> failwith ""
      end
      | t when t.token_type = Star -> begin
        match b.left_expr, b.right_expr with
        (* Num * Num *)
        | IntConst a, IntConst b ->
          let (name, _) = gen_mult_op (string_of_int a) (string_of_int b) gen in
          gen, name
        | IntConst a, e ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let (name, _offset) = gen_mult_op (string_of_int a) (var new_gen temp_name) new_gen in
          new_gen, name
        | e, IntConst a ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let (name, _offset) = gen_mult_op (string_of_int a) (var new_gen temp_name) new_gen in
          new_gen, name
        | le, re ->
          let (new_gen, left_temp_name) = gen_from_expr gen le in
          let new_gen, right_temp_name = gen_from_expr new_gen re in
          let name, _ = gen_mult_op left_temp_name right_temp_name new_gen in
          new_gen, name
      end
      | t when t.token_type = LessThan -> begin
        match b.left_expr, b.right_expr with
        | e, IntConst a ->
          let (new_gen, temp_name) = gen_from_expr gen e in
          let instr =  sprintf "%s < %s" (var new_gen temp_name) (string_of_int a) in
          new_gen, instr
        | _ -> failwith "sdsdsdsd"
      end
      | _ -> print_string "HERE"; print_newline (); print_token b.operator; failwith "todo : implement this operator for binary expression"
    end
    | Logical l -> begin
      match l.operator with
      | t when t.token_type = And -> begin
        let new_gen, left = gen_from_expr gen l.left_expr in
        let new_gen, right = gen_from_expr new_gen l.right_expr in
        let result, _ = gen_and_op left right new_gen in
        new_gen, result
      end
      | t when t.token_type = Or -> begin
        let new_gen, left = gen_from_expr gen l.left_expr in
        let new_gen, right = gen_from_expr new_gen l.right_expr in
        let result, _ = gen_or_op left right new_gen in
        new_gen, result
      end
      | _ -> failwith "dont understand bro"
    end
    | IntConst x -> gen, (string_of_int x)
    | Bool b -> gen, (string_of_bool b)
    | Var s -> gen, s
    | Call c -> 
      let ident = match c.callee with
      | Var s -> s
      | _ -> failwith "rip" in
      (* TODO: Handle all arguments *)
      let new_gen, tmp_name = gen_from_expr gen (List.nth c.arguments 0) in
      let temp_name, _ = alloc_temp_var new_gen in
      let new_gen = gen_copy_ident temp_name (sprintf "%s(%s)" ident tmp_name) new_gen in
      new_gen, temp_name
    | IfElse ie ->
      let result_name, _ = alloc_temp_var gen in
      let new_gen = emit (sprintf "let %s;" result_name) gen in 
      let new_gen, condition = gen_from_expr new_gen ie.condition in
      let new_gen = emit ("if (" ^ (var new_gen condition) ^ ") {") new_gen in
      let new_gen, then_res = gen_from_expr new_gen ie.then_branch in
      let new_gen = gen_assign result_name then_res new_gen in
      let new_gen = emit "} else {" new_gen in
      let new_gen, else_res = gen_from_expr new_gen ie.else_branch in
      let new_gen = gen_assign result_name else_res new_gen in

      let new_gen = emit "}" new_gen in
      new_gen, result_name


    | Unary u ->
      let new_gen, temp_name = gen_from_expr gen u.expr in
      let result, _ = gen_not_op temp_name gen in
      new_gen, result

    | e -> printf "%s \n" (string_of_expr e);
        failwith "todo: handle this expression in generator"

  and gen_from_stmt gen (ast: statement) = match ast with
    | LetDecl e ->
          (* Check if var has already been allocated *)
          let _offset = if is_alloc_var gen e.identifier then
            Hashtbl.find gen.variables e.identifier
          else
          (* Allocate the variable to keep track of it *)
            alloc_var e.identifier gen
          in begin match e.expr with
          | IntConst x -> 
              let (new_gen, _) = gen_from_expr gen e.expr in
              let new_gen = gen_assign e.identifier (string_of_int x) new_gen in
              new_gen
          | _ -> 
              (* Compute what we want to store in it *)
              let (new_gen, name) = gen_from_expr gen e.expr in
              let new_gen = gen_assign e.identifier name new_gen in
              new_gen
          end
    | Print e -> begin
      match e with
      | Var v -> gen_print v gen
      | _ -> gen
    end
    | Expression _ -> failwith "todo: implement Expression Codegen"
    | FunctionDecl f ->
      let dummy_generator = new_generator "functiondecl.js" in
      let rec inner gen stmts = match stmts with
      | [] -> gen
      | s :: rest ->
        let next = gen_from_stmt gen s in
        inner next rest
      in
      let final = inner dummy_generator f.body in
      let body_instructions = final.instructions in
      let new_gen = gen_def_function f.name f.params body_instructions gen in
      new_gen
    | Return r -> 
      let dummy_generator = new_generator "functiondecl.js" in
      let g, return_str = gen_from_expr dummy_generator r.value in
      emit (sprintf "%s\n return %s" g.instructions return_str) gen

  and codegen gen (ast: statement list) : string = 
    let _stmt = List.nth ast 0 in
    let rec inner gen stmts = match stmts with
      | [] -> gen
      | s :: rest ->
        let next = gen_from_stmt gen s in
        inner next rest
    in
    let final = inner gen ast in
    let output = generate_begin ^ generate_entrypoint ^  final.instructions ^ generate_end ^ generate_exit in
    output
end
*)
open Tilde

(* Only deal with 64bit ints at the moment *)
let i64_dt = DataType.create I64 0
let void_dt = DataType.create Void 0

(* Global context *)
let g_module = Module.create Architecture.X86_64 TargetSystem.Windows
let init_proto = function_create g_module 0 i64_dt 0 false
let init_func = function_build g_module init_proto "init" 0
let printf_handle = tb_extern_create g_module "printf"

let variables = Hashtbl.create 100

(* type value = 
  | Int of int *)

module Tilde = struct
  let alloc_var identifier =
    (* Create local *)
    let reg = tb_inst_local init_func 8 8 in
    (* Store *)
    Hashtbl.add variables identifier reg;
    reg

  let gen_int_const x =
    tb_inst_sint init_func i64_dt (Signed.Int64.of_int x)
  
  let gen_add_op a b =
    Instructions.inst_add init_func a b AssumeNSW

  let gen_from_expr = function
    | Binary b -> begin
      match b.operator with
      | t when t.token_type = Plus -> (
        match b.left_expr, b.right_expr with
        | IntConst a, IntConst b ->
          let a = gen_int_const a
          and b = gen_int_const b in
          let sum = gen_add_op a b in
          sum
        | _ -> failwith "todo"
      )
      | _ -> failwith "todo"
    end
    | _ -> failwith "todo"
  let gen_from_stmt = function
    | LetDecl ld ->
        (* Allocate variable *)
        let name = alloc_var ld.identifier in
        let value = gen_from_expr ld.expr in
        (*TB_API TB_Reg tb_inst_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits align); *)
        let _ = tb_inst_store init_func i64_dt name value 8 in
        ()
    | FunctionDecl _ -> failwith "todo"
    | Print e -> (
      match e with
      | Var v ->
        let format_string = tb_inst_cstring init_func "sum: %lld\n" in
        let value = Hashtbl.find variables v in
        let x = tb_inst_load init_func i64_dt value 8 in
        let arr = Ctypes.CArray.make Ctypes.int 2 in
        Ctypes.CArray.set arr 0 format_string;
        Ctypes.CArray.set arr 1 x; 
        let _ = tb_inst_ecall init_func void_dt printf_handle 2 (Ctypes.CArray.start arr) in
        ()
      | _ -> failwith "todo"
    ) 

    | Expression _ -> failwith "todo"
    | Return _ -> failwith "todo"
  let codegen (ast: statement list) =
    (* Generate code for each statement *)
    List.iter (fun stmt -> gen_from_stmt stmt; ()) ast;
    let return_code = tb_inst_sint init_func i64_dt (0 |> Signed.Int64.of_int) in
    (* Return exit code from main function *)
    Instructions.inst_return init_func return_code;
    (* Compilation of main function and module *)
    let _ = function_compile g_module init_func in
    let _ = module_compile g_module in
    let _ = module_export g_module "./test_x64.obj" true in
    Helpers.print_hashtbl variables;
    ()

end

(* module JS = Make (Js_backend.CodeGen)
module X64 = Make (X64_backend.CodeGen) *)

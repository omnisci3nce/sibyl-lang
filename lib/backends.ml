open Parser
open Codegen
open Printf

module Make (CG : CodeGenerator) = struct
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
      | _ -> print_string "HERE"; print_newline ();  failwith "todo : implement this operator for binary expression"
    end
    | IntConst x -> gen, (string_of_int x)
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
    | s -> print_stmt s; failwith "todo: handle this statement in generator"

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

module JS = Make (Js_backend.CodeGen)
module X64 = Make (X64_backend.CodeGen)
(* module C = Make (Dummy_backend.CodeGen) *)
(* module Tilde = Make (Tilde_backend.CodeGen) *)

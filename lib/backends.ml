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
          let _ = gen_copy_ident temp_name (var gen b) gen in
          let (name, _offset) = gen_plus_op (string_of_int a) (var gen temp_name) gen in
          gen, name

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
        | _ -> failwith "Cant compare against non integer constant"
      end
      | t -> failwith ("todo : implement '" ^ t.lexeme ^ "' operator for binary expression")
    end
    | IntConst x -> gen, (string_of_int x)
    | Var s -> gen, s
    | Call c -> 
      let ident = match c.callee with
      | Var s -> s
      | _ -> failwith "rip" in
      (* TODO: Handle all arguments *)
      let new_gen, tmp_name = gen_from_expr gen (List.nth c.arguments 0) in
      let temp_name, _ = alloc_temp_var new_gen in
      let _ = gen_copy_ident temp_name (sprintf "%s(%s)" ident tmp_name) new_gen in
      new_gen, temp_name
    | e -> printf "%s \n" (string_of_expr e);
        failwith ("todo: handle " ^ (string_of_expr_type e) ^ "expression in generator")

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
            let _ = gen_assign e.identifier (string_of_int x) new_gen in
            new_gen
        | _ -> 
            (* Compute what we want to store in it *)
            let (new_gen, name) = gen_from_expr gen e.expr in
            let _ = gen_assign e.identifier name new_gen in
            new_gen
        end
        | Print e -> begin
          match e with
          | Var v -> let _ =  gen_print v gen in gen
          | _ -> gen
        end
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
      let args = List.map (fun param -> param.token) f.params in
      let new_gen = gen_def_function f.name args body_instructions gen in
      new_gen
    | Return r -> 
      let dummy_generator = new_generator "functiondecl.js" in
      let g, return_str = gen_from_expr dummy_generator r.value in
      emit (sprintf "%s\n return %s" g.instructions return_str) gen
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
    let output = gen_begin ^ gen_entrypoint ^  final.instructions ^ gen_end ^ gen_exit in
    output
end

module JS = Make (Js_backend.CodeGen)
(* module X64 = Make (X64_backend.CodeGen) *)
(* module C = Make (Dummy_backend.CodeGen) *)
(* module Tilde = Make (Tilde_backend.CodeGen) *)

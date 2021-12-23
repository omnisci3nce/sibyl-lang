(* Evaluate Paper code with OCaml  *)

open Parser
open Printf

(* OCaml Runtime Values *)
type value =
  Int of int
  | Bool of bool

let string_of_value = function
  | Int x -> sprintf "Int %d" x
  | Bool b -> if b then "True" else "False"

let rec evaluate (expr: expr) = match expr with
  | IntConst x -> Int x
  | Bool b -> Bool b
  | Unary u -> begin
      match u.operator.token_type with
      | Bang -> (
        match u.expr with
        | Bool b -> Bool (not b)
        | _ -> failwith "dunno"
        )
        | _ -> failwith ""
    end
  | Binary e ->
    let left = evaluate e.left_expr in
    let right = evaluate e.right_expr in
    (match left, right with
    | Int a, Int b -> 
      begin
        match e.operator.token_type with
        | Plus -> Int (a + b)
        | Star -> Int (a * b)
        | Minus -> Int (a - b)
        | _ -> failwith ""
      end
    | Bool a, Bool b ->
      begin
        match e.operator.token_type with
        | EqualEqual -> Bool (a = b)
        | _ -> Bool true
      end
    | _ -> failwith "types dont match or coerce"
    )
  | Grouping e -> evaluate e.expr
  | _ -> failwith "Unhandled expression"

let test_interpret () =
  let t = Lexer.tokenise "false == !true \n" in
  printf "Tokens: \n"; List.iter Lexer.print_token t; print_newline ();
  let e, _ = parse_expression t in
  print_string "Expression: "; print_endline (string_of_expr e);
  let v = evaluate e in
  Printf.printf "Output: %s\n" (string_of_value v)
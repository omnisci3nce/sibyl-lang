(* Evaluate Paper code with OCaml  *)

open Parser

(* OCaml Runtime Values *)
type value =
  Int of int

let string_of_value = function
  | Int x -> Printf.sprintf "Int %d" x

let add left right = match left, right with
  | Int x, Int y -> Int (x + y)
let mult left right = match left, right with
  | Int x, Int y -> Int (x * y)
let sub left right = match left, right with
  | Int x, Int y -> Int (x - y)

let rec evaluate (expr: expr) = match expr with
  | IntConst x -> Int x
  | Binary e ->
    let left = evaluate e.left_expr in
    let right = evaluate e.right_expr in
    begin
      match e.operator.token_type with
      | Plus -> add left right
      | Star -> mult left right
      | Minus -> sub left right
      | _ -> failwith ""
    end
  | _ -> failwith ""

let test_interpret () =
  let t = Lexer.tokenise "10 * 10\n" in
  let e, _ = parse_expression t in
  print_endline (string_of_expr e);
  let v = evaluate e in
  Printf.printf "Output: %s\n" (string_of_value v)
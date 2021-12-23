(* Evaluate Paper code with JS  *)

open Parser

(* JS Runtime Values *)
type value =
  Number of int

let string_of_value = function
  | Number x -> Printf.sprintf "Number %d" x

let add left right = match left, right with
  | Number x, Number y -> Number (x + y)
let mult left right = match left, right with
  | Number x, Number y -> Number (x * y)
let sub left right = match left, right with
  | Number x, Number y -> Number (x - y)

let rec evaluate (expr: expr) = match expr with
  | IntConst x -> Number x
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
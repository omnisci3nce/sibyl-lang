(* Turn AST into assembly *)

open Lexer
open Parser
let codegen (ast: statement list) : string = 
  let stmt = List.nth ast 0 in
  match stmt with
  | Expression e -> string_of_expr e
  | _ -> ""

let test_gen () = 
  let s = "let a = 10 + 10\n" in
  let asm = s |> tokenise |> parse |> codegen in
  print_endline asm
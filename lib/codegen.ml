(* Turn AST into assembly *)

open Lexer
open Parser

type generator = {
  variables: (string, int) Hashtbl.t;
  filepath: string;
  channel: out_channel
}

let new_generator filename =
  let filepath = (Filename.chop_extension filename) ^ ".s" in
  {
    variables = Hashtbl.create 100;
    filepath;
    channel = open_out filepath
  }

let close_generator generator = close_out generator.channel

let codegen (ast: statement list) : string = 
  let stmt = List.nth ast 0 in
  match stmt with
  | Expression e -> string_of_expr e
  | _ -> ""

let test_gen () = 
  let s = "let a = 10 + 10\n" in
  let asm = s |> tokenise |> parse |> codegen in
  print_endline asm
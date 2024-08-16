open Lexing
(** Drives the parser *)

exception SyntaxError of string

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | SyntaxError msg ->
      let error_msg = Printf.sprintf "%s: %s\n" (print_error_position lexbuf) msg in
      Error error_msg
  | Parser.Error ->
      let error_msg = Printf.sprintf "%s: syntax error\n" (print_error_position lexbuf) in
      Error error_msg

open Ast

let string_of_expr expr = match expr with Int i -> "Int " ^ string_of_int i

let string_of_stmt stmt =
  match stmt with
  | Let s -> Printf.sprintf "(%d) Let %s = %s" s.loc.pos_lnum s.var_name (string_of_expr s.bindee)
  | FuncDecl -> failwith "TODO"

let print_ast prog =
  List.iter
    (fun toplevel -> match toplevel with Stmt stmt -> print_endline (string_of_stmt stmt))
    prog

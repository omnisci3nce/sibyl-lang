type token_type =
  (* single char tokens *)
  LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS

  (* Literals *)
  | Identifier
  | STRING
  | NUMBER

  (* Keywords *)
  | LET

  | EOF

type literal_type = NumberLiteral of int | StringLiteral of string

type token = {
  lexeme: string;
  literal: literal_type option;
  line: int;
  token_type: token_type;
}

type lexer_context = {
  source: string;
  start: int;
  current: int;
  line: int;
}

let run a = ()

let run_prompt () =
  while true do
    print_string "> ";
    let input = read_line () in
    run input;
  done

exception Err of string

let tokenise (stream: char Stream.t)  =
  let tokens = ref [] in
  Stream.iter (fun c -> 
    match c with
    | '(' -> tokens := [LPAREN] @ !tokens
    | _ -> raise (Err "I don't know how to handle this")
  ) stream;
  !tokens

let run_file filename =
  let channel = open_in filename in
  let stream = Stream.from
    (fun line_count ->
      try Some (input_char channel) with End_of_file -> None) in
  tokenise stream;
  ()


(* Run program *)
let () = match Array.length Sys.argv with
  | 0 -> run_prompt ()
  | 1 -> run_file (Array.get Sys.argv 0)
  | _ -> print_endline "Usage: paper [script]"; exit 64;

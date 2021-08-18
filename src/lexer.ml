type token_type =
  (* single char tokens *)
  LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Minus
  | Plus

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

let run_prompt () =
  while true do
    print_string "> ";
    let input = read_line () in
    run input;
  done

let run_file filename =
  let channel = open_in filename in
  ()


(* Run program *)
let () = match Array.length Sys.argv with
  | 0 -> run_prompt ()
  | 1 -> run_file (Array.get Sys.argv 0)
  | _ -> print_endline "Usage: paper [script]"; exit 64;

type token_type =
  (* single char tokens *)
  LeftParen
  | RightParen
  | Dot
  | Minus
  | Plus

  (* Literals *)
  | Identifier
  | String
  | Number

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

let run _ = ()

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
    | '(' -> tokens := [LeftParen] @ !tokens
    | _ -> raise (Err "I don't know how to handle this")
  ) stream;
  !tokens

let run_file filename =
  let channel = open_in filename in
  let stream = Stream.from
    (fun _ ->
      try Some (input_char channel) with End_of_file -> None)
    in
    tokenise stream


(* Run program *)
let () = match Array.length Sys.argv with
  | 0 -> run_prompt ()
  | 1 -> 
    let _ = run_file (Array.get Sys.argv 0) in
    print_string "Hello"
  | _ -> print_endline "Usage: paper [script]"; exit 64;

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
  | Let
  | Constraint
  | Module

  | EOF

type literal_type = NumberLiteral of int | StringLiteral of string

type location = {
  line: int;
  column: int
}

type token = {
  lexeme: string;
  literal: literal_type option;
  location: location;
  token_type: token_type;
}

let print_token (token: token): unit =
  print_string (
    match token.token_type with
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
    | Plus       -> "Plus"
    | Number -> begin match token.literal with | Some NumberLiteral a -> Int.to_string a | _ -> "" end
    | _ -> ""
  );
  print_newline ()


type lexer_context = {
  source: string;
  start: int;   (* first character in the lexeme being scanned *)
  current: int; (* character currently being considered *)
  line: int;
}

let is_at_end ctx = ctx.current >= String.length ctx.source

let run _ = ()

let run_prompt () =
  while true do
    print_string "> ";
    let input = read_line () in
    run input;
  done

exception Err of string


let is_alpha c = let code = Char.code c in
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

let scan_indentifier s = s

let scan_next ctx _tokens =
  let c = String.get ctx.source ctx.current in
  (* print_char c; print_newline (); *)
  match c with
  | '(' -> print_string "LeftParen"; ctx
  | ')' -> print_string "RightParen"; ctx
  | '+' -> print_string "Plus"; ctx
  | '*' -> print_string "Multiply"; ctx
  | c when is_alpha c -> let _s = scan_indentifier "let" in ctx
  | _ -> ctx

let scan_identifier x = x

let advance ctx = { ctx with current = ctx.current + 1 }

let tokenise (source: string): token list =
  let ctx: lexer_context = {
    source;
    start = 0;
    current = 0;
    line = 0;
  } in
  let tokens = ref [] in
  let rec loop ctx =
    if is_at_end ctx then
      ctx
    else
      let _t = scan_next ctx tokens in
      loop (advance ctx) in
    

  let _final = loop ctx in


  !tokens

let read_whole_file filename =
  let channel = open_in filename in
  let s = really_input_string channel (in_channel_length channel) in
  close_in channel;
  s

let run_file filename =
  filename |> read_whole_file |> tokenise |> List.iter print_token
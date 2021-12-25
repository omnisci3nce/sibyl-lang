(* Convert a source code file into tokens *)

type token_type =
  (* single char tokens *)
  LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Dot
  | Minus
  | Plus
  | Star
  | Slash
  | Equal
  | GreaterThan
  | LessThan
  | Bang
  | Comma

  (* two char tokens *)
  | EqualEqual
  | GreaterEqual
  | LesserEqual
  | BangEqual

  (* Literals *)
  | Identifier
  | String
  | Number

  (* Keywords *)
  | Let
  | True
  | False
  | Func
  | Constrain
  | If
  | Else
  | Return

  | EOF

let keywords = [
  LeftParen, "LeftParen";
  RightParen, "RightParen";
  LeftBrace, "LeftBrace";
  RightBrace, "RightBrace";
  Plus, "Plus";
  Minus, "Minus";
  Dot, "Dot";
  Star, "Star";
  Slash, "Slash";
  Equal, "Equal";
  Comma, "Comma";
  Let, "Let";
  Identifier, "Ident";
  Number, "Number";
  String, "String";
  True, "True";
  False, "False";
  Func, "Func";
  EqualEqual, "EqualEqual";
  BangEqual, "BangEqual";
  LessThan, "LessThan";
  If, "If";
  Else, "Else";
  Return, "Return"
]
let str_of_token_type t = List.assoc t keywords


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
let str_of_token t = Printf.sprintf "%s at line/col %d/%d" (str_of_token_type t.token_type) t.location.line t.location.column
    
type lexer_context = {
  source: string;
  start: int;   (* first character in the lexeme being scanned *)
  current: int; (* character currently being considered *)
  line: int;
}

let print_token (token: token): unit =
  match List.assoc_opt token.token_type keywords with
  | Some s -> print_string s
  | None -> ()

let is_at_end ctx = ctx.current >= String.length ctx.source

exception Lex_err of string

let is_alpha c = let code = Char.code c in
  (code >= Char.code('A') && code <= Char.code('Z')) ||
  (code >= Char.code('a') && code <= Char.code('z'))

let is_digit c = let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')

let is_alpha_numeric c = is_alpha c || is_digit c 

let match_next ctx c =
  c = String.get ctx.source (ctx.current + 1)

let peek ctx = String.get ctx.source (ctx.current + 1)
let advance ctx = {
  ctx with 
  current = ctx.current + 1;
}

let scan_identifier ctx = 
  let rec scan_until_end (ctx: lexer_context) (acc: string) : (lexer_context * string) =
    let c = String.get ctx.source ctx.current in
    if is_alpha_numeric (peek ctx) then
      scan_until_end (advance ctx) (acc ^ Char.escaped c)
    else (ctx, (acc ^ Char.escaped c)) in
    
  let output_ctx, output_acc = scan_until_end ctx "" in
  (output_ctx, output_acc)

let scan_number ctx = 
  let rec scan_until_end ctx acc =
    let c = String.get ctx.source ctx.current in
    if is_digit (peek ctx) then
      scan_until_end (advance ctx) (acc ^ Char.escaped c)
    else (ctx, (acc ^ Char.escaped c)) in
    
  let output_ctx, output_acc = scan_until_end ctx "" in
  (output_ctx, (int_of_string output_acc))

let add_token ttype s lit tokens line col =
  let new_token: token = {
    lexeme = s;
    literal = lit;
    location = {
      line; column = col
    };
    token_type = ttype;
  } in
  tokens := [new_token] @ !tokens

let scan_next ctx tokens =
  let c = String.get ctx.source ctx.current in
  match c with
  | '(' -> add_token LeftParen "(" None tokens ctx.line ctx.start; ctx
  | ')' -> add_token RightParen ")" None tokens ctx.line ctx.start; ctx
  | '{' -> add_token LeftBrace "(" None tokens ctx.line ctx.start; ctx
  | '}' -> add_token RightBrace ")" None tokens ctx.line ctx.start; ctx
  | ',' -> add_token Comma "," None tokens ctx.line ctx.start; ctx
  | '+' -> add_token Plus "+" None tokens ctx.line ctx.start; ctx
  | '-' -> add_token Minus "-" None tokens ctx.line ctx.start; ctx
  | '*' -> add_token Star "*" None tokens ctx.line ctx.start; ctx
  | '!' -> (
      match match_next ctx '=' with
      | true  -> add_token BangEqual "!=" None tokens ctx.line ctx.start; { ctx with current = ctx.current + 1 } (* consume that 2nd char *)
      | false -> add_token Bang "!" None tokens ctx.line ctx.start; ctx
  )
  | '=' -> (
      match match_next ctx '=' with
      | true  -> add_token EqualEqual "==" None tokens ctx.line ctx.start; { ctx with current = ctx.current + 1 } (* consume that 2nd char *)
      | false -> add_token Equal "=" None tokens ctx.line ctx.start; ctx
  )
  | '<' -> (
      match match_next ctx '=' with
      | true  -> add_token LesserEqual "<=" None tokens ctx.line ctx.start; { ctx with current = ctx.current + 1 } (* consume that 2nd char *)
      | false -> add_token LessThan "<" None tokens ctx.line ctx.start; ctx
  )
  | '/' -> 
    let rec comment_out c =
      if is_at_end c || Char.equal (peek c) '\n' then
        c
      else
        c |> advance |> comment_out in
    if (peek ctx) = '/' then
      let new_ctx = comment_out ctx in
      { new_ctx with line = new_ctx.line + 1 }
     else
      let _ = add_token Slash "/" None tokens ctx.line ctx.start in
      ctx
  | c when is_alpha c -> 
    let new_ctx, indent_string = scan_identifier ctx in
    begin
    match indent_string with
    | "let" -> add_token Let "let" None tokens ctx.line ctx.start; new_ctx
    | "fn" -> add_token Func "fn" None tokens ctx.line ctx.start; new_ctx
    | "constrain" -> add_token Constrain "constrain" None tokens ctx.line ctx.start; new_ctx
    | "true" -> add_token True "constrain" None tokens ctx.line ctx.start; new_ctx
    | "false" -> add_token False (str_of_token_type False) None tokens ctx.line ctx.start; new_ctx
    | "if" -> add_token If (str_of_token_type False) None tokens ctx.line ctx.start; new_ctx
    | "else" -> add_token Else (str_of_token_type False) None tokens ctx.line ctx.start; new_ctx
    | "return" -> add_token Return (str_of_token_type False) None tokens ctx.line ctx.start; new_ctx
    | _ -> add_token Identifier indent_string None tokens ctx.line ctx.start; new_ctx
    end
  | c when is_digit c ->
    let new_ctx, num = scan_number ctx in
    add_token Number (string_of_int num) (Some (NumberLiteral num)) tokens ctx.line ctx.start; new_ctx
  | ' ' -> ctx
  | '\n' -> { ctx with line = ctx.line + 1 }
  | _ -> raise (Lex_err ("Unknown character: " ^ (Char.escaped c) ^ " on line " ^ (string_of_int ctx.line)))

let scan_identifier x = x


let tokenise_ (source: string) : lexer_context * token list =
  let ctx: lexer_context = {
      source;
      start = 0;
      current = 0;
      line = 0;
    } in
    let tokens = ref [] in
    let rec loop ctx = (* loop until we're at the end of the source code *)
      if is_at_end ctx then
        ctx
      else
        let t = scan_next ctx tokens in
        loop (advance t) in
  let final = loop ctx in
  final, !tokens

let tokenise (source: string): token list =
  let _ctx, tokens = tokenise_ source in
  List.rev tokens

let read_whole_file filename =
  let channel = open_in filename in
  let s = really_input_string channel (in_channel_length channel) in
  close_in channel;
  s
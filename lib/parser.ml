open Lexer

(* Types *)
type expr =
  | Literal of literal
  | Assign of string * expr
  | Binary of token * expr * expr
  | Grouping
  | Unit
and literal = (string * literal_type)

let at_end t = List.length t = 0


let rec print_expr e =
  match e with
  | Assign (s, e) -> print_string ("Assign \"" ^ s ^ "\" to "); print_expr e
  | Binary (t, a, b) -> print_string "BinaryExpr "; print_string "("; print_token t; print_expr a; print_endline ","; print_expr b; print_newline (); print_endline ")"
  | Literal (_s, l) -> begin 
    match l with
    | NumberLiteral x -> print_string "NumberLiteral: "; print_int x
    | _ -> raise (Err "havent implemented non number literals yet")
  end
  | _ -> print_endline ""

let rec parse (tokens: token list) : expr =
  if List.length tokens = 0 then Unit else
  (* let t = List.nth tokens 0 in *)
  match tokens with
  | t :: next :: rest when t.token_type = Let && next.token_type = Identifier -> Assign (next.lexeme, (parse rest))
  | a :: b :: c :: _ when a.token_type = Number && b.token_type = Plus && c.token_type = Number ->
          Binary (b, Literal (a.lexeme, (Option.get a.literal)), Literal (c.lexeme, (Option.get c.literal)))
  | _ :: rest -> parse rest
  (* | _ :: [] -> Unit *)
  | _ -> Unit

let testfile_expected =
  Assign (
    "a",
    Binary (
      { token_type = Plus; lexeme = "+"; literal = None; location = { line = 0; column = 0}},
      Literal ("10", NumberLiteral 10),
      Literal ("10", NumberLiteral 10)
    )
  )

let test_parse = 
  let s = "let a = 10 + 10\n" in
  let ts = tokenise s in 
  let ast = parse ts in
  print_expr ast;
  assert (ast = testfile_expected)
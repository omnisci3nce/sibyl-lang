(* Parse tokens from the lexer for syntax correctness and convert to an abstract syntax tree *)

open Lexer

(* Types *)
type expr =
  | Literal of literal
  | Assign of string * expr
  | Binary of token * expr * expr
  | Variable of token
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

let rec parse (statements: expr list ref) (tokens: token list) : expr =
  if List.length tokens = 0 then Unit else
  (* let t = List.nth tokens 0 in *)
  match tokens with
  | t :: rest when t.token_type = Let -> begin
    match rest with
      | next :: _ when next.token_type = Identifier -> 
        let expr = Assign (next.lexeme, (parse (statements) rest)) in
        statements := [expr] @ !statements;
        expr
      | _ -> raise (Err "Expected identifier after 'let'")
    end
  | a :: b :: c :: _ when b.token_type = Plus && a.token_type = Number -> begin
    if a.token_type != c.token_type then raise (Err "type of left side of Plus operator must be the same as type of right side") else
      let expr = Binary (b, Literal (a.lexeme, (Option.get a.literal)), Literal (c.lexeme, (Option.get c.literal))) in
      statements := [expr] @ !statements;
      expr
  end
  | _ :: rest -> parse (statements) rest
  (* | _ :: [] -> Unit *)
  | _ -> Unit

let testfile_expected = [
  Assign (
    "a",
    Binary (
      { token_type = Plus; lexeme = "+"; literal = None; location = { line = 0; column = 0}},
      Literal ("10", NumberLiteral 10),
      Literal ("10", NumberLiteral 10)
    )
  );
  Assign (
    "b",
    Binary (
      { token_type = Plus; lexeme = "+"; literal = None; location = { line = 1; column = 0}},
      Variable ({token_type = Identifier; lexeme = "b"; literal = None; location = { line = 1; column = 0}}),
      Literal ("20", NumberLiteral 20)
    )
  )
]

let test_parse = 
  let s = "let a = 10 + 10\n let b = a + 20\n" in
  let ts = tokenise s in 
  let stmts = ref [] in
  let _ast = parse stmts ts in (* list of top-most nodes *)
  let prog = !stmts in
  List.iter print_expr prog;
  assert (!stmts = testfile_expected)
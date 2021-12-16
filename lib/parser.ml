(* Parse tokens from the lexer for syntax correctness and convert to an abstract syntax tree *)

open Lexer

(* Types *)
type expr =
  | Literal of literal
  | Assign of { identifier: string; expr: expr }
  | Binary of { left_expr: expr; operator: token; right_expr: expr}
  | Unary of { operator: token; expr: expr }
  | Unit
and literal = (string * literal_type)

type statement =
  | Expression of expr
  | Print of expr

type program = statement list

let rec string_of_expr e = match e with
    | Literal l -> begin 
      match l with
      | (_s, NumberLiteral i) -> "num_literal " ^ (string_of_int i)
      | _ -> ""
    end
    | Assign _ -> "assign"
    | Binary b -> "Binary (" ^ b.operator.lexeme ^ " " ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")"
    | _ -> "unknown expr"

let print_stmt s = match s with
  | Expression e ->
    begin
    match e with
    | Assign a -> print_string ("Assign to '" ^ a.identifier ^ "' "); print_endline (string_of_expr a.expr)
    | Binary b -> print_endline ("Binary (" ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")")
    | _ -> print_string "Unknown"
    end
  | _ -> print_endline "dunno"



let at_end t = List.length t = 0

let parse_expression tokens = match tokens with
  | n1 :: op :: n2 :: rest when n1.token_type = Number && n2.token_type = Number && op.token_type = Plus ->
    rest, Binary { left_expr = Literal (n1.lexeme, Option.get n1.literal); operator = op; right_expr = Literal (n2.lexeme, Option.get n2.literal) }
  | _ -> raise (Err "dunno how to parse this expression")

let parse_statement tokens = match tokens with
  (* starts a let *)
  | { token_type = Let; _ } :: { token_type = Identifier; lexeme; _} :: { token_type = Equal; _} :: rest ->
    let remaining_t, ex = parse_expression rest in
      Expression (
        Assign { identifier = lexeme; expr = ex}), remaining_t
  (* If its not a declaration, we assume its an expression *)
  | _ -> Expression Unit, []

let rec loop (acc: statement list) (ts: token list) =
  match ts with
  | [] -> acc
  | ts -> let stmt, remaining_tokens = parse_statement ts in
    loop (stmt :: acc) remaining_tokens
let parse (tokens: token list) : statement list =
  let stmts = loop [] tokens in
  List.rev stmts

let test_parse () = 
  let s1 = "let a = 10 + 10\n" in
  let _s2 = "let a = 10 + 10\n let b = 10 + 20\n" in
  let _s3 = "let a = 10 + 10\n let b = a + 20\n" in
  let tokens = tokenise s1 in 
  let ast = parse tokens in
  print_string "Statements: "; print_int (List.length ast); print_newline ();
  List.iter print_stmt ast;
  ()
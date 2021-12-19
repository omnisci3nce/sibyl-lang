(* Parse tokens from the lexer for syntax correctness and convert to an abstract syntax tree *)

open Lexer

(* Types *)
type expr =
  | Literal of literal
  | Let of { identifier: string; expr: expr }
  | Binary of { left_expr: expr; operator: token; right_expr: expr}
  | Unary of { operator: token; expr: expr }
  | Var of string
  | If of expr * expr * expr
  | Unit
  and literal = (string * literal_type)

  type statement =
    | Expression of expr
    | Print of expr
    
    type program = statement list
    
    type value =
      | Int
      
exception Syntax_error of string

let rec string_of_expr e = match e with
    | Literal l -> begin 
      match l with
      | (_s, NumberLiteral i) -> "num_literal " ^ (string_of_int i)
      | _ -> ""
    end
    | Let e -> "Let (" ^ string_of_expr e.expr
    | Binary b -> "Binary (" ^ b.operator.lexeme ^ " " ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")"
    | Unit -> "Unit"
    | _ -> "unknown expr"

let print_stmt s = match s with
  | Expression e ->
    begin
    match e with
    | Let a -> print_string ("Assign to '" ^ a.identifier ^ "' "); print_endline (string_of_expr a.expr)
    | Binary b -> print_endline ("Binary (" ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")")
    | _ -> print_string (string_of_expr e)
    end
  | Print e -> begin match e with
    | Var v -> print_endline ("Print " ^ v)
    | _ -> print_endline "dunno"
  end

let at_end t = List.length t = 0

let rec parse_primary tokens =
  match tokens with
  | h :: r  when h.token_type = Number -> Literal (h.lexeme, Option.get h.literal), r
  | h :: r when h.token_type = Identifier -> Var h.lexeme, r
  | _ :: r -> Unit, r
  | _ -> failwith " stuck"

and parse_factor tokens = 
let (expr, remaining) = parse_primary tokens in

  match remaining with
  | op :: rest when op.token_type = Star ->
    let (ex, rem) = parse_primary rest in
    Binary { left_expr = expr; operator = op; right_expr = ex }, rem
  | _ -> (expr, remaining)
    
and parse_term tokens = 
  let (expr, remaining) = parse_factor tokens in
  match remaining with
  | op :: rest when op.token_type = Plus ->
    let ex, _rem = parse_term rest in
    Binary { left_expr = expr; operator = op; right_expr = ex }, remaining
  | op :: rest -> 
    let ex, _rem = parse_term rest in
    Binary { left_expr = expr; operator = op; right_expr = ex }, remaining
| _ -> expr, remaining

and parse_expression tokens = match tokens with
  | h :: rest when h.token_type = Let -> begin
    match rest with
    | { token_type = Identifier; _} :: { token_type = Equal; _} :: remaining ->
      let expr, rem = parse_expression remaining in
      Let { identifier = "let"; expr = expr}, rem
    | _ -> failwith "cooked"
    
  end
  | h :: _a :: rest when h.token_type = Identifier && h.lexeme = "print" ->
    parse_expression rest
  | _ -> parse_term tokens

let parse_statement tokens = match tokens with
  (* starts a let *)
  | { token_type = Let; _ } :: { token_type = Identifier; lexeme; _} :: { token_type = Equal; _} :: rest ->
    let ex, remaining_t = parse_expression rest in
      Expression (
        Let { identifier = lexeme; expr = ex}), remaining_t
  (* | { token_type = Identifier; lexeme = "print"; _} :: ({ token_type = Identifier; _} as v) :: rest ->
    Print (Var v.lexeme), rest *)
  (* If its not a declaration, we assume its an expression *)
  | _ -> Expression Unit, []

let rec loop (acc: statement list) (ts: token list) =
  match ts with
  | [] -> acc
  | ts -> let stmt, remaining_tokens = parse_statement ts in
  (* List.iter print_token remaining_tokens; print_newline (); *)
    loop (stmt :: acc) remaining_tokens
  let parse (tokens: token list) : statement list =
  let stmts = loop [] tokens in
  List.rev stmts

let test_parse () = 
  let s1 = "let a = 10 + 10 * 10\n" in
  let tokens = tokenise s1 in 
  let _ast = parse tokens in
  ()
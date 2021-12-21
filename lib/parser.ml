(* Parse tokens from the lexer for syntax correctness and convert to an abstract syntax tree *)

open Lexer

(* Types *)
type expr =
  | IntConst of int
  (* | StringConst of string *)
  | Let of { identifier: string; expr: expr }
  | Binary of { left_expr: expr; operator: token; right_expr: expr}
  | Unary of { operator: token; expr: expr }
  | Grouping of { expr: expr }
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
    | IntConst i ->  "num_literal " ^ (string_of_int i)
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

let match_next tokens (token_types: token_type list) = match tokens with
  | [] -> None
  | hd :: _ -> begin
    match List.find_opt (fun tt -> tt = hd.token_type) token_types with
    | Some _ -> Some hd
    | _ -> None
  end

let rec parse_primary tokens =
  match tokens with
  | h :: r  when h.token_type = Number -> 
    let i = begin match Option.get h.literal with
    | NumberLiteral a -> a
    | _ -> failwith "int expected"
    end in
    IntConst i, r
  | h :: r when h.token_type = Identifier -> Var h.lexeme, r
  | h :: r when h.token_type = LeftParen ->
    begin
      let (expr, remaining) = parse_expression r in
      print_endline (string_of_expr expr);
      print_endline "Remaining: ";
      List.iter print_token remaining;
      match remaining with
      | h :: r when h.token_type = RightParen -> Grouping { expr = expr }, r
      | _ -> failwith "right paren expected"
    end
  | _ :: r -> Unit, r
  | _ -> failwith " stuck"

and parse_factor tokens = 
  let (expr, remaining) = parse_primary tokens in
  match match_next remaining [Star; Slash] with
  | Some t ->
    let (ex, rem) = parse_primary (List.tl remaining) in
    Binary { left_expr = expr; operator = t; right_expr = ex }, rem
  | _ -> (expr, remaining)
    
and parse_term tokens = 
  let (expr, remaining) = parse_factor tokens in
  match match_next remaining [Plus; Minus] with
  | Some t ->
      let ex, rem = parse_term (List.tl remaining) in
      Binary { left_expr = expr; operator = t; right_expr = ex }, rem
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
  let s1 = "let a = 10 + 10 + 10\n" in
  let tokens = tokenise s1 in 
  let _ast = parse tokens in
  ()
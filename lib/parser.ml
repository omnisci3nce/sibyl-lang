(* Parse tokens from the lexer for syntax correctness and convert to an abstract syntax tree *)

open Lexer
open Printf

(* Types *)
type expr =
  | IntConst of int
  | Bool of bool
  | Binary of { left_expr: expr; operator: token; right_expr: expr}
  | Logical of { left_expr: expr; operator: token; right_expr: expr}
  | Unary of { operator: token; expr: expr }
  | Grouping of { expr: expr }
  | Var of string
  | IfElse of { condition: expr; then_branch: expr; else_branch: expr }
  | Call of { callee: expr; arguments: expr list }
  | Unit
  and literal = (string * literal_type)

type func_param = { type_annot: string option; token: token }
type statement =
  | Expression of expr
  | LetDecl of { identifier: string; type_annot: string option; expr: expr }
  | FunctionDecl of { name: string; arity: int; params: func_param list; return_type_annot: string option; body: statement list }
  | Print of expr
  | Return of { value: expr }
  
type program = statement list

exception Syntax_error of string

let rec string_of_expr e = match e with
    | IntConst i ->  "IntConst " ^ (string_of_int i)
    | Bool b -> if b then "True" else "False"
    | Binary b -> "Binary (" ^ b.operator.lexeme ^ " " ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")"
    | Logical _l -> "Logical"
    | Unary _ -> "Unary"
    | Grouping e -> Printf.sprintf "Grouping (%s)" (string_of_expr e.expr) 
    | Var s -> "Var: " ^ s
    | IfElse ie -> "IfElse " ^ (Printf.sprintf "If %s Then %s Else %s" (string_of_expr ie.condition) (string_of_expr ie.then_branch) (string_of_expr ie.else_branch)) 
    | Call _ -> "Call"
    | Unit -> "Unit"

let string_of_expr_type e = match e with
  | IntConst _ -> "IntConst" 
    | Bool _ -> "Bool"
    | Binary _ -> "Binary" 
    | Logical _ -> "Logical"
    | Unary _ -> "Unary"
    | Grouping _ -> "Grouping"
    | Var _ -> "Var"
    | IfElse _ -> "IfElse"
    | Call _ -> "Call"
    | Unit -> "Unit"


let rec print_stmt s = match s with
  | Expression e ->
    begin
    match e with
    | Binary b -> print_endline ("Binary (" ^ string_of_expr b.left_expr ^ ", " ^ string_of_expr b.right_expr ^ ")")
    | Call c -> printf "Call: %s\n" (string_of_expr c.callee)
    | _ -> print_string (string_of_expr e)
    end
  | Print e -> begin match e with
    | Var v -> printf "Print %s" v
    | _ -> print_endline ("Expr " ^ string_of_expr e)
  end
  | LetDecl a -> print_endline "Let"; print_string (string_of_expr a.expr)
  | FunctionDecl f -> printf "FunctionDecl: %s \n" f.name; List.iter (fun s -> print_stmt s) f.body
  | Return _ -> print_endline "Return"

let at_end t = List.length t = 0

let match_next tokens (token_types: token_type list) = match tokens with
  | [] -> None
  | hd :: _ -> begin
    match List.find_opt (fun tt -> tt = hd.token_type) token_types with
    | Some _ -> Some hd
    | _ -> None
  end

let rec parse_primary tokens =
  (* print_string "\nparse primary: "; List.iter print_token tokens ; *)
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
      (* print_endline (string_of_expr expr);
      print_endline "Remaining: "; *)
      (* List.iter print_token remaining; *)
      match remaining with
      | h :: r when h.token_type = RightParen -> Grouping { expr = expr }, r
      | _ -> failwith "right paren expected"
    end
  | h :: r when h.token_type = True -> Bool true, r
  | h :: r when h.token_type = False -> Bool false, r
  (* | _ :: r -> Unit, r *)
  | _ -> failwith " stuck"

and parse_argument (args: expr list ) tokens =
  match List.hd tokens with
  | { token_type = Comma; _ } ->
      let next_arg, rem = parse_argument args (List.tl tokens) in
      next_arg @ args, rem
  | { token_type = RightParen; _ } -> args, tokens
  | _ -> 
    let ex, rem = parse_expression tokens in
    let next, rem = parse_argument args rem in
    [ex] @ next, rem
and parse_params tokens = match tokens with
  | h :: r when h.token_type = Identifier -> begin
      match match_next r [Comma] with
      | Some _ ->
        let next, rem = parse_params (List.tl r) in
        [h] @ next, rem
      | _ -> [h], r
    end
  | _ -> [], tokens
and parse_func_decl_params tokens = match tokens with
    | h :: r when h.token_type = Identifier -> (
      match match_next r [Colon] with
      | Some _ -> (
        match (List.tl r) with
        | t :: r when h.token_type = Identifier ->
          let param = { type_annot = Some t.lexeme; token = h} in
          let next, rem = parse_func_decl_params r in
          [param] @ next, rem
        | _ -> failwith "missing type annotation after colon"
        )
      | None -> (
        let param = { type_annot = None; token = h} in
        match match_next r [Comma] with
          | Some _ ->
            let next, rem = parse_func_decl_params (List.tl r) in
            [param] @ next, rem
          | _ -> [param], r
      )
    )
    | h :: r when h.token_type = Comma -> parse_func_decl_params r
    | _ -> [], tokens
and parse_call tokens =
  let (expr, remaining) = parse_primary tokens in
  match match_next remaining [LeftParen] with
  (* We have an opening parenthesis next so we know it's a function call *)
  | Some _ -> (
    let args, remaining = parse_argument [] (List.tl remaining) in
    (* print_newline ();
    print_string "args tokens: "; *)
    (* List.iter print_token remaining; *)
    match match_next remaining [RightParen] with
      | Some _ -> Call { callee = expr; arguments = args }, List.tl remaining
      | None -> failwith "closing parenthesis expected"
    )
  (* No opening parenthesis so we just pass the expression back *)
  | None -> expr, remaining
and parse_unary tokens =
  match match_next tokens [Bang] with
  | Some t -> (* we have a unary *)
    let (right, rem) = parse_unary (List.tl tokens) in
    Unary { operator = t; expr = right }, rem
  | _ -> parse_call tokens

and parse_factor tokens = 
  let (expr, remaining) = parse_unary tokens in
  match match_next remaining [Star; Slash] with
  | Some t ->
    let (ex, rem) = parse_factor (List.tl remaining) in
    Binary { left_expr = expr; operator = t; right_expr = ex }, rem
  | _ -> (expr, remaining)
    
and parse_term tokens = 
  let (expr, remaining) = parse_factor tokens in
  match match_next remaining [Plus; Minus] with
  | Some t ->
      let ex, rem = parse_term (List.tl remaining) in
      Binary { left_expr = expr; operator = t; right_expr = ex }, rem
  | _ -> expr, remaining
and parse_comparison tokens =
  let expr, remaining = parse_term tokens in
  match match_next remaining [LessThan; LesserEqual; GreaterThan; GreaterEqual] with
    | Some t ->
      let ex, rem = parse_comparison (List.tl remaining) in
      Binary { left_expr = expr; operator = t; right_expr = ex }, rem
    | _ -> expr, remaining

and parse_equality tokens =
    let (expr, remaining) = parse_comparison tokens in
    match match_next remaining [EqualEqual; BangEqual] with
    | Some t ->
      let ex, rem = parse_comparison (List.tl remaining) in
      Binary { left_expr = expr; operator = t; right_expr = ex }, rem
      | _ -> expr, remaining
      
      
and parse_logical_and tokens = 
  let expr, remaining = parse_equality tokens in
  match match_next remaining [And] with
  | Some t ->
    let ex, rem = parse_equality (List.tl remaining) in
    Logical { left_expr = expr; operator = t; right_expr = ex }, rem
  | None -> expr, remaining

and parse_logical_or tokens =
    let expr, remaining = parse_logical_and tokens in
    match match_next remaining [Or] with
    | Some t -> 
      let ex, rem = parse_logical_and (List.tl remaining) in
      Logical { left_expr = expr; operator = t; right_expr = ex }, rem
    | None -> expr, remaining

and parse_expression tokens = match tokens with
  | h :: rest when h.token_type = If -> (
    let condition_expr, rem = parse_logical_or rest in
    match match_next rem [Then] with
    | Some _ -> (
      let then_expr, rem = parse_logical_or (List.tl rem) in
        match match_next rem [Else] with
        | Some _ -> (
          let else_expr, rem = parse_logical_or (List.tl rem) in
          IfElse { condition = condition_expr; then_branch = then_expr; else_branch = else_expr}, rem)
        | None -> failwith "must provide else branch"
    )
    | None -> failwith "must provide then branch" 
  )
  | h :: a :: rest when a.token_type = Identifier && h.lexeme = "print" ->
    parse_expression rest
  | _ -> parse_logical_or tokens

and parse_function tokens : (statement list * func_param list * string option * token list) =
  match match_next tokens [LeftParen] with
  | Some _ -> begin
    let rest = List.tl tokens in    
    let params, remaining = parse_func_decl_params rest in
    (* List.iter print_token params;
    print_endline "remaining: ";
    List.iter print_token remaining; *)

    match match_next remaining [RightParen] with
    | Some _ -> begin
        (* Return type annotation *)
        let rest = List.tl remaining in
        let rest, return_type_annot = (match rest with
        | { token_type = Colon; _ } :: rt :: after_rt when rt.token_type = Identifier -> after_rt, Some rt.lexeme
        | _ -> rest, None) in
        (* print_string (match return_type_annot with | Some "bool" -> "return bool" | _ -> "return unknown"); *)
        (* function body *)
        match match_next rest [LeftBrace] with
        | Some _ ->
          let statements = ref [] in
          let rec parse_f_body tokens = (match tokens with
            | [] -> failwith "you need a closing brace"
            | h :: r when h.token_type = RightBrace -> r
            | _ -> 
              let stmt, rem = parse_statement tokens in
              statements := !statements @ [stmt];
              parse_f_body rem
          ) in
          let rem = parse_f_body (List.tl rest) in
          !statements, params, return_type_annot, rem
        | None -> failwith "no function body"
      end
    | None -> failwith "parse arguments to be implemented"
  end
  | None -> failwith "Expected left paren after a function keyword"

and parse_statement tokens = match tokens with
  (* starts a let statement *)
  | { token_type = Let; _ } :: { token_type = Identifier; lexeme; _} :: { token_type = Equal; _} :: rest ->
    let ex, remaining_t = parse_expression rest in
      LetDecl { identifier = lexeme; type_annot = None; expr = ex}, remaining_t
  | { token_type = Let; _ } :: { token_type = Identifier; lexeme; _} :: { token_type = Colon; _} :: t :: { token_type = Equal; _} :: rest when t.token_type = Identifier ->
    let ex, remaining_t = parse_expression rest in
      LetDecl { identifier = lexeme; type_annot = Some t.lexeme; expr = ex}, remaining_t
  (* starts a print statement *)
  | { token_type = Identifier; lexeme = "print"; _} :: ({ token_type = Identifier; _} as v) :: rest ->
    Print (Var v.lexeme), rest
  | { token_type = Identifier; lexeme = "print"; _} :: rest ->
    let expr, rem = parse_expression rest in
    Print (expr), rem
  (* starts a function declaration statement *)
  | { token_type = Func; _} :: { token_type = Identifier; lexeme; _} :: rest ->
    let body, params, return_type_annot, remaining = parse_function rest in
    FunctionDecl { name = lexeme; arity = List.length params; params = params; return_type_annot = return_type_annot; body = body }, remaining
  | { token_type = Return; _} :: rest ->
      let ex, rest = parse_expression rest in
      Return { value = ex }, rest
  | { token_type = If; _} :: rest -> (
    match match_next rest [LeftParen] with
    | Some _ ->  (
      let _condition, rem = parse_expression (List.tl rest) in
      (* print_string "Condition "; print_endline (string_of_expr condition); *)
      match match_next rem [RightParen] with
      | Some _ -> (
        let _then_branch, rem = parse_statement (List.tl rem) in
        (* print_string "Then branch "; print_stmt then_branch; *)
        match match_next rem [Else] with
        | Some _ -> 
          let else_branch, rem = parse_statement (List.tl rem) in
          print_string "Else branch "; print_stmt else_branch;
          (* TODO clean this up *)
          Print Unit, rem
        | _ -> failwith "xd"
      )
      | None  -> failwith "if condition needs closing bracket"
    )
    | None -> failwith "if condition needs to be inside parentehses"
  )
  (* If its not a declaration, we assume its an expression *)
  | ts -> 
    let ex, rest = parse_expression ts in
    Expression ex, rest

let rec loop (acc: statement list) (ts: token list) =
  match ts with
  | [] -> acc
  | ts -> let stmt, remaining_tokens = parse_statement ts in
    loop (stmt :: acc) remaining_tokens
let parse (tokens: token list) : statement list =
  let stmts = loop [] tokens in
  List.rev stmts

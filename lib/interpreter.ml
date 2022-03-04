(* Evaluate Sibyl code with OCaml  *)

open Parser
open Printf

(* OCaml Runtime Values *)
type value =
  Int of int
  | Bool of bool
  | Unit

type env = (string, value) Hashtbl.t

exception RuntimeError of string

let string_of_value = function
  | Int x -> sprintf "Int %d" x
  | Bool b -> if b then "True" else "False"
  | Unit -> "Unit"

let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (string_of_value y))

let test_value_equality = function
  | Bool b -> b
  | _ -> failwith "this is not of type bool"

let rec evaluate func_env var_env (expr: expr) = match expr with
  | IntConst x -> Int x
  | Bool b -> Bool b
  | Unary u -> (
      match u.operator.token_type with
      | Bang -> (
        match u.expr with
        | Bool b -> Bool (not b)
        | Grouping g -> Bool (g.expr |> evaluate func_env var_env |> test_value_equality |> not)
        | _ -> failwith ""
      )
      | _ -> failwith ":O"
    )
  | Binary e ->
    let left = evaluate func_env var_env e.left_expr in
    let right = evaluate func_env var_env e.right_expr in
    (match left, right with
    | Int a, Int b -> (
        match e.operator.token_type with
        | Plus -> Int (a + b)
        | Star -> Int (a * b)
        | Minus -> Int (a - b)
        | EqualEqual -> Bool (a = b)
        | LessThan -> Bool (a < b)
        | LesserEqual -> Bool (a <= b)
        | GreaterThan -> Bool (a > b)
        | GreaterEqual -> Bool (a >= b)
        | _ -> failwith "operator"
      )
    | Bool a, Bool b -> (
        match e.operator.token_type with
        | EqualEqual -> Bool (a = b)
        | _ -> Bool true
      )
    | _ -> failwith "[Runtime Error] types dont match or coerce"
    )
  | Grouping e -> evaluate func_env var_env e.expr
  | Call c ->
      let args_length = List.length c.arguments in
      let name = begin match c.callee with
      | Var v -> v
      | _ -> failwith "Call callee has to be Var expression (I think)" 
      end in
      let (args, body) = try Hashtbl.find func_env name with
                          Not_found -> failwith (sprintf "Couldn't find function '%s'" name) in
      let func_arity = List.length args in
      if args_length != func_arity then
        raise (RuntimeError (sprintf "Expected %d arguments but got %d" func_arity args_length)) else
      let scoped_env = Hashtbl.copy var_env in
      List.iteri (fun i arg  ->
        let open Lexer in
        let expr = (List.nth c.arguments i) in
        let value = evaluate func_env var_env expr in
        Hashtbl.add scoped_env arg.token.lexeme (value)  
      ) args;
      let output = ref (Unit) in
      List.iter (fun s -> 
        let x = evaluate_stmt func_env scoped_env s in
        match x with
        (* return value *)
        | Some v -> output := v; ()
        | None -> ()
      ) body;
      !output
  | Unit -> failwith "Unhandled Unit"
  | IfElse ie -> (
      (* eval condition *)
      let cond = evaluate func_env var_env ie.condition in
      match cond with
      | Bool b -> if b then evaluate func_env var_env ie.then_branch else evaluate func_env var_env ie.else_branch
      | _ -> failwith "expected condition to evaluate to a boolean"
  )
  | Logical l -> (
    let left = evaluate func_env var_env l.left_expr in
    match left with
    | Bool b1 -> (
      if l.operator.token_type = Or
        then (if b1 then Bool b1 else evaluate func_env var_env l.right_expr)
      else
        let right = evaluate func_env var_env l.right_expr in
          match right with
          | Bool b2 -> Bool (b1 && b2)
          | _ -> failwith "Logical operators (right one) must be used on booleans"
        
    )
    | _ -> failwith "Logical operators must be used on booleans"
  )
  | Var v -> try Hashtbl.find var_env v with
              Not_found -> failwith (sprintf "Couldn't find variable '%s': (%s)" v (string_of_expr expr))

and evaluate_stmt (func_env: (string, func_param list * statement list) Hashtbl.t) (var_env: (string, value) Hashtbl.t) stmt : value option =
  match stmt with
  | LetDecl l -> 
      let value = evaluate func_env var_env l.expr in
      Hashtbl.add var_env l.identifier value;
      None
  | FunctionDecl f ->
      let fv = (f.params, f.body) in
      Hashtbl.add func_env f.name fv;
      None
  | Expression e -> let _ = evaluate func_env var_env e in None
  | Print e -> let value = evaluate func_env var_env e in print_endline (string_of_value value); None
  | Return r -> Some (evaluate func_env var_env r.value)

let clock: statement list = [
  Return { value = IntConst (int_of_float (Unix.time ()))}
]

let test_interpret () =
  let var_env = Hashtbl.create 10 in
  let func_env = Hashtbl.create 10 in
  (* Hashtbl.add func_env "clock" ([], clock); *)
  let t = Lexer.tokenise "
  fn ageInSeconds(yearsOld: int): int {
    let result: int = yearsOld * 365 * 24 * 60
    return result
  }

  let me: int = ageInSeconds(24)
  print me
" in

  (* printf "Tokens: \n"; List.iter Lexer.print_token t; print_newline (); *)
  (* let _ = parse t |> Typer.typecheck in *)
  let program = parse t in
  List.iter (fun stmt ->
    (* print_string "[Statement] "; print_stmt stmt; print_newline (); *)
    let _ = evaluate_stmt func_env var_env stmt in ()
  ) program;
    ()
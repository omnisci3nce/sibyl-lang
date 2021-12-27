(* Evaluate Paper code with OCaml  *)

open Parser
open Printf

(* OCaml Runtime Values *)
type value =
  Int of int
  | Bool of bool

type env = (string, value) Hashtbl.t

let string_of_value = function
  | Int x -> sprintf "Int %d" x
  | Bool b -> if b then "True" else "False"

let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (string_of_value y))


let test_value_equality = function
  | Bool b -> b
  | Int _ -> failwith "this is not of type bool"

let rec evaluate func_env var_env (expr: expr) = match expr with
  | IntConst x -> Int x
  | Bool b -> Bool b
  | Unary u -> (
      match u.operator.token_type with
      | Bang -> (
        match u.expr with
        | Bool b -> Bool (not b)
        | _ -> failwith "dunno"
        )
        | _ -> failwith ""
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
      let name = begin match c.callee with
      | Var v -> v
      | _ -> failwith "Call callee has to be Var expression (I think)" 
      end in
      let (args, body) = Hashtbl.find func_env name in
      let scoped_env = Hashtbl.copy var_env in
      List.iteri (fun i arg  ->
        let open Lexer in
        let expr = (List.nth c.arguments i) in
        let value = evaluate func_env var_env expr in
        Hashtbl.add scoped_env arg.lexeme (value)  
      ) args;
      let output = ref (Bool true) in
      List.iter (fun s -> 
        let x = evaluate_stmt func_env scoped_env s in
        match x with
        (* return value *)
        | Some v -> output := v; ()
        | None -> ()
      ) body;
      !output
  | Unit -> failwith "Unhandled Unit"
  | IfElse _ -> failwith "Unhandled IfElse"
  | Var v -> Hashtbl.find var_env v

and evaluate_stmt (func_env: (string, Lexer.token list * statement list) Hashtbl.t) (var_env: (string, value) Hashtbl.t) stmt : value option = match stmt with
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


let test_interpret () =
  let var_env = Hashtbl.create 10 in
  let func_env = Hashtbl.create 10 in
  let t = Lexer.tokenise "
  fn fib(n) {
    let next1 = n - 1
    let next2 = n - 2
    if (n < 2)
      let a = n
    else
      let a = fib(next1) + fib(next2)
    return a
  }
  let a = fib(35)
  print a
  " in
  printf "Tokens: \n"; List.iter Lexer.print_token t; print_newline ();
  let program = parse t in
  List.iter (fun stmt ->
    print_stmt stmt;
    let _ = evaluate_stmt func_env var_env stmt in ()
  ) program
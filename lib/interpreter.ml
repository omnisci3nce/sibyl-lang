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

let rec evaluate func_env var_env (expr: expr) = match expr with
  | IntConst x -> Int x
  | Bool b -> Bool b
  | Unary u -> begin
      match u.operator.token_type with
      | Bang -> (
        match u.expr with
        | Bool b -> Bool (not b)
        | _ -> failwith "dunno"
        )
        | _ -> failwith ""
    end
  | Binary e ->
    let left = evaluate func_env var_env e.left_expr in
    let right = evaluate func_env var_env e.right_expr in
    (match left, right with
    | Int a, Int b -> 
      begin
        match e.operator.token_type with
        | Plus -> Int (a + b)
        | Star -> Int (a * b)
        | Minus -> Int (a - b)
        | _ -> failwith ""
      end
    | Bool a, Bool b ->
      begin
        match e.operator.token_type with
        | EqualEqual -> Bool (a = b)
        | _ -> Bool true
      end
    | _ -> failwith "[Runtime Error] types dont match or coerce"
    )
  | Grouping e -> evaluate func_env var_env e.expr
  | Call c ->
      let name = begin match c.callee with
      | Var v -> v
      | _ -> failwith "dunno var" 
      end in
      let (_args, body) = Hashtbl.find func_env name in
      List.iter (fun s -> evaluate_stmt func_env var_env s) body;
      Bool true
  | Unit -> failwith "unhandled unit"
  | If _ -> failwith "unhandled if"
  | Var v -> Hashtbl.find var_env v
  (* | _ -> failwith "Unhandled expression" *)

and evaluate_stmt func_env var_env stmt = match stmt with
  | LetDecl l -> 
      let value = evaluate func_env var_env l.expr in
      Hashtbl.add var_env l.identifier value
  | FunctionDecl f ->
      let fv = (f.arguments, f.body) in
      Hashtbl.add func_env f.name fv
  | Expression e -> let _ = evaluate func_env var_env e in ()
  | Print e -> let value = evaluate func_env var_env e in print_endline (string_of_value value)
  | IfElse _ -> ()
  | Return _ -> ()

let print_hashtbl = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x (string_of_value y))

let test_interpret () =
  let var_env = Hashtbl.create 10 in
  let func_env = Hashtbl.create 10 in
  let t = Lexer.tokenise "
  fn hello() {
    let a = 7
    print a
  }
  hello()
  hello()
  " in
  (* printf "Tokens: \n"; List.iter Lexer.print_token t; print_newline (); *)
  let program = parse t in
  (* print_int (List.length program); print_newline (); *)
  List.iter (fun stmt ->
    evaluate_stmt func_env var_env stmt;
    (* print_stmt stmt; *)
  ) program;
  print_hashtbl var_env
  (* print_string "Expression: "; print_endline (string_of_expr e); *)
  (* let v = evaluate e in *)
  (* Printf.printf "Output: %s\n" (string_of_value v) *)
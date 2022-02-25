open Parser
open Lexer

let rec _check_vars func_env var_env prog  =

  let rec check_expr fenv venv expr = match expr with
    | Var v -> begin match Hashtbl.find_opt venv v with
      | Some _ -> expr
      | None -> failwith ("Couldnt find variable " ^ v ^ " in scope")
    end
    | Binary b ->
        let _ = check_expr fenv venv b.left_expr in
        let _ = check_expr fenv venv b.right_expr in
        expr
    | Call { callee; arguments } -> 
      let ident = match callee with
          | Var s -> s
          | _ -> failwith "todo" in
      let _ = begin
        match Hashtbl.find_opt fenv ident with
        | Some _ -> ident
        | None -> failwith ("Couldnt find function " ^ ident ^ " in scope")
      end in
      let _ = List.iter (fun e -> let _ = check_expr fenv venv e in ()) arguments in expr

    | _ -> expr in
  List.iter (fun stmt ->
    let _ = match stmt with
    | Expression expr -> let _ = check_expr func_env var_env expr in stmt
    | FunctionDecl { body; params; arity; _} ->
      let scoped_env = Hashtbl.create arity in
      List.iter (fun p -> Hashtbl.add scoped_env p.lexeme true) params;
      let _ = _check_vars func_env scoped_env body in stmt
    | LetDecl { expr; _ } -> let _ = check_expr func_env var_env expr in stmt
    | _ -> stmt in
    ()
  ) prog;
  prog

let check_vars = 
  let var_env = Hashtbl.create 10 in
  let func_env = Hashtbl.create 10 in
  _check_vars func_env var_env

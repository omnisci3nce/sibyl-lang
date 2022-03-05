open Parser

type typ =
  | TInt
  | TBool

type expr
type typed_expr = { typ: typ; expr: expr }
type typed_tree = typed_expr

exception TypeError of string

module StaticEnv = struct
  (* variable with name and type *)
  type t = (string * typ) list

  let empty = []

  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith "unbound variable"
  
  let extend env x ty =
    (x, ty) :: env
end

let type_annot_to_typ = function
  | "bool" -> TBool
  | "int"  -> TInt
  | _ -> failwith "unknown type annotation"

let rec typeof env = function
  | Bool _ -> TBool
  | IntConst _ -> TInt
  | Var s -> StaticEnv.lookup env s
  | Binary { left_expr; right_expr; operator; _ } -> typeof_binop env operator left_expr right_expr
  | Grouping g -> typeof env g.expr
  | Logical l -> typeof_logical env l.left_expr l.right_expr
  | IfElse { condition; then_branch; else_branch } ->
    let _ = match typeof env condition with | TBool -> () | _ -> raise (TypeError "if/else condition must be of type bool") in
    let then_type = typeof env then_branch
    and else_type = typeof env else_branch in
    if then_type != else_type then raise (TypeError "return type of then and else branches must match")
    else then_type
  | Call c -> begin match c.callee with
              | Var s -> StaticEnv.lookup env s
              | _ -> failwith "todo"
              end
  | _ -> failwith "TODO: implement typeof for this expression type"

and typeof_binop env op e1 e2 = let open Lexer in
  let t1, t2 = typeof env e1, typeof env e2 in
  match op.token_type, t1, t2 with
  | Plus, TInt, TInt
  | Minus, TInt, TInt
  | Star, TInt, TInt
  | Slash, TInt, TInt -> TInt
  | _ -> failwith "dunno"

and typeof_logical env e1 e2 =
  let t1, t2 = typeof env e1, typeof env e2 in
  if t1 = TBool && t2 = TBool then TBool else failwith "both sides of logical operator must be Bool type"

let unwrap_opt = function
  | Some v -> v
  | None -> failwith "cooked"

let typecheck (prog: program) : program =
  (* let check_expr env expr = match expr with
    | _ -> let _ = type_of env expr in env
  in *)
  let check_stmt env stmt = match stmt with
    | LetDecl { identifier; type_annot; expr; _ } ->
      let annotated_type = type_annot_to_typ (unwrap_opt type_annot) in
      let new_env = StaticEnv.extend env identifier annotated_type in
      let expr_type = typeof new_env expr in
      if expr_type != annotated_type then raise (TypeError "let decl type mismatch")
      else new_env
      
    | FunctionDecl f ->
        let annotated_type = type_annot_to_typ (unwrap_opt f.return_type_annot) in
        StaticEnv.extend env f.name annotated_type
    | _ -> env
  in
  let _ = List.fold_left check_stmt (StaticEnv.empty) prog in
  (* List.iter (fun stmt -> check_stmt prog; *)
  prog

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

let type_of env = function
  | Bool _ -> TBool
  | IntConst _ -> TInt
  | Var s -> StaticEnv.lookup env s
  | _ -> failwith "yeah nah"

let typecheck (prog: program) : program =
  let env = StaticEnv.empty in
  (* Iterate through each statement *)
  let check_stmt env stmt = match stmt with
    | LetDecl { identifier; _ } ->
      (* Check that identifier isn't already defined *)
      let typ = StaticEnv.lookup env identifier in
      
    | FunctionDecl _ -> ()
    | _ -> ()
  in
  (* List.iter (fun stmt -> check_stmt prog; *)
  prog
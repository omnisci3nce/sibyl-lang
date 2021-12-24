module Context = Map.Make (String)

type expr =
  Variable of string
  | Abstraction of { param: string; body: expr }
  | Application of { func: expr; argument: expr }

type value = Closure of { context: value Context.t; param: string; body: expr }

let rec interpret context expr =
  match expr with
  | Variable name -> Context.find name context
  | Abstraction { param; body } -> Closure { context; param; body }
  | Application { func; argument } ->
    let argument = interpret context argument in
    let (Closure { context; param; body }) = interpret context func in
    interpret (Context.add param argument context) body
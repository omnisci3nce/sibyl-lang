type e

type constant =
  | ConstInt

type expr =
  | Let 
  | Const of constant

type typ =
  | TInt

type typed_expr = { typ: typ; expr: expr }

type typed_tree = typed_expr

let to_typed (_prog: Parser.program) : typed_tree = { typ = TInt; expr = Let }
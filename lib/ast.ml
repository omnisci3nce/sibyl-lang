type loc = Lexing.position
type unary_op = Complement | Negate
type binary_op = Add | Subtract | Multiply | Divide
type literal = Int of int
type builtin_type = I32 | F32 | Bool | Char

type expr =
  | Int of int
  | Var of string
  | UnaryOp of { rhs : expr; operator : unary_op }
  | BinaryOp of { lhs : expr; rhs : expr; operator : binary_op }
  | IfElse of { condition : expr; if_expr : expr; else_expr : expr }

and stmt =
  | Let of { loc : loc; var_name : string; bindee : expr }
  | Expression of expr
  | Return of { value : expr }
  | DebugPrint of expr  (** Prints out all internal compiler knowledge about an expr at runtime *)

and toplevel_item =
  | Stmt of stmt
  | FuncDecl of { loc : loc; func_name : string; arity : int; body : stmt list }

type program = toplevel_item list

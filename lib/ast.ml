type loc = Lexing.position
type unary_op = Negate
type binary_op = Add | Subtract | Multiply | Divide
type literal = Int of int

type expr = Int of int
(* | Literal of literal *)
(* | BinaryOp of { lhs: expr; rhs: expr; operator: binary_op } *)
(* | IfElse of { condition: expr; if_expr: expr; else_expr: expr } *)

and stmt =
  | Let of { loc : loc; var_name : string; bindee : expr } (* Let binding "let x = 5" *)
  | FuncDecl (* TODO: arguments *)

and toplevel_item = Stmt of stmt

type builtin_type = I32 | F32 | Bool | Char
type program = toplevel_item list

module Typer = struct end
(** This module helps take an untyped AST and produce a typed AST *)

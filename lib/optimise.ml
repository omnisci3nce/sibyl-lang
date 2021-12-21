open Parser

type pass =
| ConstantFold

let constant_fold ast =
  let inner (expr: expr) = match expr with
    | Binary bop -> begin
      match bop.left_expr, bop.operator, bop.right_expr with
      | IntConst n1, { token_type = Plus; _}, IntConst n2 ->
        let sum = n1 + n2 in
          IntConst sum
      | _ -> expr
    end
    (* | Let e -> Let { e with expr = inner e.expr } *)
    | _ -> expr
  in
  let handle_node (stmt: statement) = match stmt with
  | Expression e -> Expression (inner e) (* *)
  | _ -> stmt (* if its not a expression statement do nothing *)
  in
  List.map handle_node ast

let optimise (ast: Parser.program) : Parser.program = 
  constant_fold ast
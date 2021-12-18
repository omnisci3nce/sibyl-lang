open Parser

type pass =
| ConstantFold

let constant_fold ast =
  let rec inner (expr: expr) = match expr with
    | Binary bop -> begin
      match bop.left_expr, bop.operator, bop.right_expr with
      | Literal (_s1, NumberLiteral n1), { token_type = Plus; _}, Literal (_s2, NumberLiteral n2) ->
        let sum = n1 + n2 in
          Literal (string_of_int sum, NumberLiteral sum)
      | _ -> expr
    end
    | Let e -> Let { e with expr = inner e.expr }
    | _ -> expr
  in
  let handle_node (stmt: statement) = match stmt with
  | Expression e -> Expression (inner e) (* *)
  | _ -> stmt (* if its not a expression statement do nothing *)
  in
  List.map handle_node ast

let optimise (ast: Parser.program) : Parser.program = 
  constant_fold ast
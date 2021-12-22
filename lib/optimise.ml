open Parser

type pass =
| ConstantFold

let constant_fold ast =
  let rec inner (expr: expr) = match expr with
    | Binary bop -> begin
      match bop.left_expr, bop.operator, bop.right_expr with
      | IntConst n1, { token_type = Plus; _}, IntConst n2 ->
        let sum = n1 + n2 in
          IntConst sum
      | IntConst n1, { token_type = Star; _}, IntConst n2 ->
        let sum = n1 * n2 in
          IntConst sum
      | le, { token_type = Star; _ }, re ->
        let left = inner le in
        let right = inner re in
        begin match left, right with
        | IntConst n1, IntConst n2 ->
          let sum = n1 * n2 in
          IntConst sum
        | _ -> expr
        end
      | _ -> expr
    end
    | Grouping e -> inner e.expr
    | _ -> expr
  in
  let handle_node (stmt: statement) = match stmt with
  | LetDecl a -> LetDecl { a with expr = inner a.expr }
  | _ -> stmt
  in
  List.map handle_node ast

let optimise (ast: Parser.program) : Parser.program = 
  constant_fold ast
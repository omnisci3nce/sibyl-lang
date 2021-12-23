open Paper

let pprint_expr ppf e = let open Parser in match e with
  | IntConst x -> Fmt.pf ppf "IntConst %d" x
  | Binary b -> Fmt.pf ppf "Binary %s ( %s %s)" (Lexer.str_of_token b.operator) (string_of_expr b.left_expr) (string_of_expr b.right_expr)
  | _ ->  Fmt.pf ppf ""

let expr_eq a b = let open Parser in match a, b with
  | Binary e1, Binary e2 -> e1.operator.token_type = e2.operator.token_type (* ignore location etc *)
  | _ -> a = b

let expr_testable = Alcotest.testable pprint_expr expr_eq


let pprint_stmt ppf stmt = let open Parser in match stmt with
  | Expression _ -> Fmt.pf ppf ""
  | LetDecl _ -> Fmt.pf ppf ""
  | FunctionDecl f -> Fmt.pf ppf "Function %s -- args: %d -- stmts: %d"
      f.name (List.length f.arguments) (List.length f.body)
  | Print _ -> Fmt.pf ppf ""
let stmt_eq a b = a = b
let stmt_testable = Alcotest.testable pprint_stmt stmt_eq


let test_at_end () =
  Alcotest.(check bool) "empty list" true (Parser.at_end []);
  Alcotest.(check bool) "non-empty list" false (Parser.at_end [1])

let make_binary l r lex op_t = Parser.(
  Binary { left_expr = l; operator = {
    lexeme = lex;
    literal = None;
    location = { line = 0; column = 0 };
    token_type = op_t;
  }; right_expr = r
})

let test_add_two_numbers () =
  let ts = Lexer.tokenise "5 + 5\n" in
  let expr, _rem = Parser.parse_expression ts in
  let expected_expr = make_binary (IntConst 5) (IntConst 5) "+" Plus in
  Alcotest.(check expr_testable) "success" expected_expr expr 

let test_declare_empty_function () =
  let source = "
  fn hello() {

  }
  " in
  let ts = Lexer.tokenise source in
  let stmt, _rem = Parser.parse_statement ts in
  let expected_output = Parser.FunctionDecl {
    name = "hello";
    arguments = [];
    body = []
  } in
  Alcotest.(check stmt_testable) "Correct function name" expected_output stmt

let test_one_statement_in_function () =
  let source = "
  fn hello() {
    let a = 10
  }
  " in
  let ts = Lexer.tokenise source in
  let stmt, _rem = Parser.parse_statement ts in
  let expected_output = Parser.FunctionDecl {
    name = "hello";
    arguments = [];
    body = [
      Parser.LetDecl {
        identifier = "a";
        expr = Parser.IntConst 10
      }
    ]
  } in
    Alcotest.(check stmt_testable) "Correct function body statement" expected_output stmt
  
(* TODO: let test_parse_params () = *)
  (* let tokens = [] *)

let test_one_param_func () =
  let source = "
  fn hello (a) {
  }
  " in
  let ts = Lexer.tokenise source in
  let stmt, _rem = Parser.parse_statement ts in
  let expected_output = Parser.FunctionDecl {
    name = "hello";
    arguments = [{
      lexeme = "a";
      literal = None;
      location = { line = 1; column = 0};
      token_type = Lexer.Identifier;
    }];
    body = []
  } in
    Alcotest.(check stmt_testable) "Correct function params" expected_output stmt

let test_function_call () =
  let source = "hello()\n" in
  let ts = Lexer.tokenise source in
  let expr, _rem = Parser.parse_expression ts in
  let expected_output = Parser.Call {
    callee = Var "hello";
    arguments = []
  } in
  Alcotest.(check expr_testable) "function call" expected_output expr

let dummy_test () =
  ()

let () =
  let open Alcotest in
  run "Parser Tests" [
    "Helper functions", [
      test_case "at_end" `Quick test_at_end
    ];
    "Arithmetic", [
      test_case "binary plus" `Quick test_add_two_numbers
    ];
    "Control flow", [
      test_case "TODO" `Quick dummy_test
    ];
    "Functions", [
      test_case "Empty function declaration" `Quick test_declare_empty_function;
      test_case "One statement in function body" `Quick test_one_statement_in_function;
      test_case "One param" `Quick test_one_param_func;
      test_case "Function call no params" `Quick test_function_call
    ];
    "Pattern matching", [
      test_case "TODO" `Quick dummy_test
    ];
    "Type definitions", [
      test_case "TODO" `Quick dummy_test
    ];
  ]
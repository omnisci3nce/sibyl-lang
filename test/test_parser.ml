open Paper

let pprint_expr ppf e = let open Parser in match e with
  | IntConst x -> Fmt.pf ppf "IntConst %d" x
  | Binary b -> Fmt.pf ppf "Binary %s ( %s %s)" (Lexer.str_of_token b.operator) (string_of_expr b.left_expr) (string_of_expr b.right_expr)
  | Call c -> 
      (* let tts = List.map (fun t -> Lexer.(str_of_token_type t.token_type)) c.arguments in
      let args = List.fold_right (fun str acc -> acc ^ ", " ^ str) tts "" in *)
      Fmt.pf ppf "Call %s " (string_of_expr c.callee) 
  | IfElse ie -> Fmt.pf ppf "If %s Then %s Else %s"
                (string_of_expr ie.condition) (string_of_expr ie.then_branch) (string_of_expr ie.else_branch)
  | _ ->  Fmt.pf ppf ""

let expr_eq a b = let open Parser in match a, b with
  | Binary e1, Binary e2 -> e1.operator.token_type = e2.operator.token_type (* ignore location etc *)
  | _ -> a = b

let expr_testable = Alcotest.testable pprint_expr expr_eq


let pprint_stmt ppf stmt = let open Parser in match stmt with
  | Expression e -> Fmt.pf ppf "Expr"; pprint_expr ppf e;
  | LetDecl l -> Fmt.pf ppf "Let %s = %s" l.identifier (string_of_expr l.expr)
  | FunctionDecl f -> Fmt.pf ppf "Function %s -- args: %d -- stmts: %d"
      f.name (List.length f.params) (List.length f.body)
  | Print _ -> Fmt.pf ppf ""
  | _ -> Fmt.pf ppf ""
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
    params = [];
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
    params = [];
    body = [
      Parser.LetDecl {
        identifier = "a";
        expr = Parser.IntConst 10
      }
    ]
  } in
    Alcotest.(check stmt_testable) "Correct function body statement" expected_output stmt
  
let test_parse_call_arguments () =
  let ts = Lexer.tokenise "hello(5)\n" in
  let expr, _ = Parser.parse_expression ts in
  let expected = Parser.Call { callee = Var "hello"; arguments = [
    IntConst 5
    (* {
      lexeme = "5";
      literal = Some (NumberLiteral 5);
      location = { line = 0; column = 0 };
      token_type = Number
    } *)
  ] } in
  Alcotest.(check expr_testable) "Correct arguments" expected expr

let test_parse_call_expr_as_argument () =
  let ts = Lexer.tokenise "hello(5 + 1)\n" in
  let expr, _ = Parser.parse_expression ts in
  let expected = Parser.Call { callee = Var "hello"; arguments = [
    Binary {
      left_expr = IntConst 5;
      operator = {
        lexeme = "+";
        literal = None;
        location = { line = 0; column = 0};
        token_type = Plus
      };
      right_expr = IntConst 1
    }]
  } in
  Alcotest.(check expr_testable) "Correct arguments" expected expr

let test_one_param_func () =
  let source = "
  fn hello (a) {
  }
  " in
  let ts = Lexer.tokenise source in
  let stmt, _rem = Parser.parse_statement ts in
  let expected_output = Parser.FunctionDecl {
    name = "hello";
    params = [{
      lexeme = "a";
      literal = None;
      location = { line = 1; column = 0};
      token_type = Lexer.Identifier;
    }];
    body = []
  } in
    Alcotest.(check stmt_testable) "Correct function params" expected_output stmt

let test_fibonacci_decl () = let open Lexer in let open Parser in
  let fibonacci_new = "
  fn fib(n) {
    let a = if (n < 2) then n
            else fib(n - 1) + fib(n - 2)
    return a
  }
  let a = fib(12)
  print a
  " in
  let ts = tokenise fibonacci_new in
  let program = parse ts in
  let expected = [
    FunctionDecl {
      name = "fib";
      params = [{
        lexeme = "n";
        literal = None;
        location = { line = 1; column = 0};
        token_type = Identifier;
      }];
      body = []
    };
    LetDecl {
      identifier = "a";
      expr = Call {
        callee = Var "a";
        arguments = [
          IntConst 12
          (* { *)
          (* lexeme = "12";
          literal = Some (NumberLiteral 12);
          location = { line = 1; column = 0};
          token_type = Number; *)
        (* } *)
        ]
      }
    };
    Print (Var "a")
  ] in
  Alcotest.(check (list stmt_testable)) "Correct function params" expected program

  
let test_function_call () =
  let source = "hello()\n" in
  let ts = Lexer.tokenise source in
  let expr, _rem = Parser.parse_expression ts in
  let expected_output = Parser.Call {
    callee = Var "hello";
    arguments = []
  } in
  Alcotest.(check expr_testable) "function call" expected_output expr

let test_return () =
  let _source = "\n" in
  Alcotest.(check pass) "" () ()

let test_if_else () = let open Lexer in let open Parser in
  let source = "let x = if 5 < 10 then 5 else 10\n" in
  let tokens = Lexer.tokenise source in
  let program = parse tokens in
  print_endline ""; List.iter (fun s -> print_stmt s; print_newline ()) program;
  let expected = [
    LetDecl {
      identifier = "x";
      expr = IfElse {
        condition = Binary {
          left_expr = IntConst 5;
          right_expr = IntConst 10;
          operator = {
            token_type = LessThan;
            lexeme = "<";
            location = { line = 0; column = 0};
            literal = None
          }
        };
        then_branch = IntConst 5;
        else_branch = IntConst 10
      }
    }
  ] in
  Alcotest.(check (list stmt_testable)) "Correct if else branches" expected program

let dummy_test () =
  ()

let () =
  let open Alcotest in
  run "Parser Tests" [
    "Helper functions", [
      test_case "at_end" `Quick test_at_end
    ];
    "Arithmetic", [
      test_case "Binary plus" `Quick test_add_two_numbers
    ];
    "Control flow", [
      test_case "IfElse" `Quick test_if_else
    ];
    "Function Declarations", [
      test_case "Empty function declaration" `Quick test_declare_empty_function;
      test_case "One statement in function body" `Quick test_one_statement_in_function;
      test_case "One param" `Quick test_one_param_func;
      test_case "Function call no params" `Quick test_function_call;
      test_case "" `Quick test_parse_call_expr_as_argument;
      test_case "return" `Quick test_return;
      (* test_case "Fibonacci" `Quick test_fibonacci_decl; *)
    ];
    "Function Calls", [
      test_case "Parse arguments" `Quick test_parse_call_arguments
    ];
    "Pattern matching", [
      test_case "TODO" `Quick dummy_test
    ];
    "Type definitions", [
      test_case "TODO" `Quick dummy_test
    ];
  ]
open Paper

let pprint_expr ppf e = let open Parser in match e with
  | IntConst x -> Fmt.pf ppf "IntConst %d" x
  | Binary b -> Fmt.pf ppf "Binary %s ( %s %s)" (Lexer.str_of_token b.operator) (string_of_expr b.left_expr) (string_of_expr b.right_expr)
  | _ ->  Fmt.pf ppf ""

let expr_eq a b = a = b
let expr_testable = Alcotest.testable pprint_expr expr_eq

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

(* let test_declare_empty_function () = *)

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
      test_case "TODO" `Quick dummy_test
    ];
    "Pattern matching", [
      test_case "TODO" `Quick dummy_test
    ];
    "Type definitions", [
      test_case "TODO" `Quick dummy_test
    ];
  ]
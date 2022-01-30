(* The tests *)
open Paper

let pprint_token_type ppf tt = Fmt.pf ppf "%s" (Lexer.str_of_token_type tt)

let token_type_eq a b = a = b
let token_type_testable = Alcotest.testable pprint_token_type token_type_eq

let token_types_only tokens = let open Lexer in List.map (fun t -> t.token_type) tokens

let test_single_int_literal () =
  let tts = Lexer.tokenise "5\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Number" [Number] tts
let test_if () =
  let tts = Lexer.tokenise "if\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "If" [If] tts
let test_return () =
  let tts = Lexer.tokenise "return\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Return" [Return] tts

let test_comparison () =
  let tts = Lexer.tokenise "true != false\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "BangEqual" [True; BangEqual; False] tts

let test_and () =
  let tts = Lexer.tokenise "true && false\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "And" [True; And; False] tts

let test_or () =
  let tts = Lexer.tokenise "false || true\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "And" [False; Or; True] tts

let test_empty_function_decl () = let open Lexer in
  let source = "
  fn hello() {

  }
  " in
  let tts = Lexer.tokenise source |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Decl Fn Hello" [Func; Identifier; LeftParen; RightParen; LeftBrace; RightBrace] tts

let test_func_with_one_param () = let open Lexer in
  let source = "fn hello (a) {}\n" in
  let tts = tokenise source |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Decl Fn Hello a" [
    Func; Identifier; LeftParen; Identifier; RightParen; LeftBrace; RightBrace
  ] tts

let test_line_numbers () = let open Lexer in
  let source = "let a = 5\nlet b = 5\n" in
  let ctx, _ = tokenise_ source in
  Alcotest.(check int) "line number advances" 2 ctx.line

let test_fibonacci () = let open Lexer in
  let fibonacci_new = "
  fn fib(n) {
    let a = if (n < 2) then n
            else fib(n - 1) + fib(n - 2)
    return a
  }
  let a = fib(12)
  print a
  " in
  let tts = Lexer.tokenise fibonacci_new |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Declare and use Fibonacci function"
  [Func; Identifier; LeftParen; Identifier; RightParen; LeftBrace;
  Let; Identifier; Equal; If; LeftParen; Identifier; LessThan; Number; RightParen; Then; Identifier;
  Else; Identifier; LeftParen; Identifier; Minus; Number; RightParen; Plus; Identifier; LeftParen; Identifier; Minus; Number; RightParen;
  Return; Identifier;
  RightBrace;
  Let; Identifier; Equal; Identifier; LeftParen; Number; RightParen;
  Identifier; Identifier
  ] tts

let () =
  let open Alcotest in
  run "Tests" [
      "basic tokenise", [ 
        test_case "Single int literal" `Quick test_single_int_literal;
        test_case "line numbers" `Quick test_line_numbers;
        test_case "&&" `Quick test_and;
        test_case "||" `Quick test_or;
        test_case "if" `Quick test_if;
        test_case "return" `Quick test_return;
     ];
      "basic comparison", [ test_case "Comparison" `Quick test_comparison ];
      "basic function declaration", [
        test_case "Empty" `Quick test_empty_function_decl;
        test_case "1 param" `Quick test_func_with_one_param;
        test_case "Fibonacci" `Quick test_fibonacci
      ]
    ]
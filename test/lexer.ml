(* The tests *)
open Paper

let pprint_token ppf = function
  | Lexer.LeftParen -> Fmt.pf ppf "LeftParen "
  | _ -> Fmt.pf ppf ""

let token_eq a b = a = b
let token_testable = Alcotest.testable pprint_token token_eq

let test_single_int_literal () =
  let ts = Lexer.tokenise "(" in
  let t = List.nth ts 0 in
  let ttype = t.token_type in
  Alcotest.(check token_testable) "left paren" LeftParen ttype

(* Run it *)
let () =
  let open Alcotest in
  run "Tests" [
      "basic tokenise", [ test_case "Left parenthesis" `Quick test_single_int_literal  ];
    ]
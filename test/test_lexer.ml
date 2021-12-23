(* The tests *)
open Paper

let pprint_token ppf tt = Fmt.pf ppf "%s" (Lexer.str_of_token_type tt)

let token_eq a b = a = b
let token_testable = Alcotest.testable pprint_token token_eq

let token_types_only tokens = let open Lexer in List.map (fun t -> t.token_type) tokens

let test_single_int_literal () =
  let ts = Lexer.tokenise "5\n" in
  let tts = token_types_only ts in
  Alcotest.(check (list token_testable)) "Number" [Number] tts

let test_comparison () =
  let ts = Lexer.tokenise "true != false\n" in
  let tts = token_types_only ts in
  Alcotest.(check (list token_testable)) "BangEqual" [True; BangEqual; False] tts

(* Run it *)
let () =
  let open Alcotest in
  run "Tests" [
      "basic tokenise", [ test_case "Single int literal" `Quick test_single_int_literal ];
      "", [ test_case "Comparison" `Quick test_comparison ]
    ]
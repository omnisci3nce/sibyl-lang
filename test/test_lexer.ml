(* The tests *)
open Paper

let pprint_token_type ppf tt = Fmt.pf ppf "%s" (Lexer.str_of_token_type tt)

let token_type_eq a b = a = b
let token_type_testable = Alcotest.testable pprint_token_type token_type_eq

let token_types_only tokens = let open Lexer in List.map (fun t -> t.token_type) tokens

let test_single_int_literal () =
  let tts = Lexer.tokenise "5\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "Number" [Number] tts

let test_comparison () =
  let tts = Lexer.tokenise "true != false\n" |> token_types_only in
  Alcotest.(check (list token_type_testable)) "BangEqual" [True; BangEqual; False] tts

(* Run it *)
let () =
  let open Alcotest in
  run "Tests" [
      "basic tokenise", [ test_case "Single int literal" `Quick test_single_int_literal ];
      "basic comparison", [ test_case "Comparison" `Quick test_comparison ]
    ]
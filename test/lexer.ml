(* The tests *)
open Paper

let pprint_token ppf = function
  | Lexer.LeftParen -> Fmt.pf ppf "LeftParen"
  | Lexer.RightParen -> Fmt.pf ppf "RightParen"
  | Lexer.Number -> Fmt.pf ppf "Number"
  | _ -> Fmt.pf ppf ""

let token_eq a b = a = b
let token_testable = Alcotest.testable pprint_token token_eq

let token_types_only tokens = let open Lexer in List.map (fun t -> t.token_type) tokens

let test_single_int_literal () =
  let ts = Lexer.tokenise "5\n" in
  let tts = token_types_only ts in
  
  Alcotest.(check (list token_testable)) "Number" [Number] tts

(* Run it *)
let () =
  let open Alcotest in
  run "Tests" [
      "basic tokenise", [ test_case "Single int literal" `Quick test_single_int_literal ];
    ]
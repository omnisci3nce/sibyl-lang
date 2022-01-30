open Paper
open Paper.Interpreter

let pprint_value ppf v = Fmt.pf ppf "%s" (string_of_value v)

let value_eq a b = a = b
let value_testable = Alcotest.testable pprint_value value_eq

let test_and () =
  let v = Hashtbl.create 10 in
  let f = Hashtbl.create 10 in
  let t1 = Lexer.tokenise "true && true\n" in
  let t2 = Lexer.tokenise "true && false\n" in
  let t3 = Lexer.tokenise "false && true\n" in
  let t4 = Lexer.tokenise "false && false\n" in
  let values = List.map (fun ts -> ts |> Parser.parse_expression |> fst |> evaluate f v) [t1; t2; t3; t4] in
  let expected = [Bool true; Bool false; Bool false; Bool false] in
  Alcotest.(check (list value_testable)) "Truth table checks out" expected values

let test_or () =
  let v = Hashtbl.create 10 in
  let f = Hashtbl.create 10 in
  let t1 = Lexer.tokenise "true || true\n" in
  let t2 = Lexer.tokenise "true || false\n" in
  let t3 = Lexer.tokenise "false || true\n" in
  let t4 = Lexer.tokenise "false || false\n" in
  let values = List.map (fun ts -> ts |> Parser.parse_expression |> fst |> evaluate f v) [t1; t2; t3; t4] in
  let expected = [Bool true; Bool true; Bool true; Bool false] in
  Alcotest.(check (list value_testable)) "Truth table checks out" expected values

let test_not () =
  let v = Hashtbl.create 10 in
  let f = Hashtbl.create 10 in
  let t1 = Lexer.tokenise "!true\n" in
  let t2 = Lexer.tokenise "!false\n" in
  let values = List.map (fun ts -> ts |> Parser.parse_expression |> fst |> evaluate f v) [t1; t2] in
  let expected = [Bool false; Bool true] in
  Alcotest.(check (list value_testable)) "Truth table checks out" expected values

let () =
  let open Alcotest in
  run "Tests" [
    "Logical Operators", [
      test_case "Logical And (&&)" `Quick test_and;
      test_case "Logical Or (||)" `Quick test_or;
      test_case "Logical Not (!)" `Quick test_not
    ];
    "Combining Logical Operators", [

    ]
  ]
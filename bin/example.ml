open Sibyl

let test_program = "\nfn main() {\n  let x = 5\n  let y = 10\n}"
(* let test_assign = "let x = 5"*)

let () =
  let lexbuf = Lexing.from_string test_program in
  Printf.printf "Convert source \"%s\" =>\n" test_program;
  let open Parse in
  match parse_program lexbuf with
  | Ok ast -> print_ast ast
  | Error msg -> print_endline ("ERROR: \n" ^ msg)

open Sibyl

let test_program = "int main() { return 5; }"

let () =
  let lexbuf = Lexing.from_string test_program in
  Printf.printf "Convert source \"%s\" ->\n" test_program;
  let open Parse in
  match parse_program lexbuf with
  | Ok ast -> print_ast ast
  | Error msg -> print_endline ("ERROR: \n" ^ msg)

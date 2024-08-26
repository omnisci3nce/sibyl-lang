[@@@warning "-32"] (* Allow unused values *)

open Sibyl

let test_program = "\nfn main() {\n  let x = 5\n  let y = 10\n}"
let test_return = "\nfn main() {\n return 5\n}"
(* let test_assign = "let x = 5"*)

let () =
  let prog_to_compile = test_return in
  let lexbuf = Lexing.from_string prog_to_compile in
  Printf.printf "Convert source \"%s\" =>\n" prog_to_compile;
  let open Parse in
  match parse_program lexbuf with
  | Ok ast ->
      print_ast ast;
      print_endline "Into Assembly =>\n";
      let asm_ast = Codegen.gen_program ast in
      List.iter (Codegen.emit_function stdout) asm_ast
      (* List.iter (fun fn -> print_endline (Asm.string_of_func_def fn)) asm_ast);*)
  | Error msg -> print_endline ("ERROR: \n" ^ msg)

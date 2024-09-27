[@@@warning "-32"] (* Allow unused values *)

open Sibyl

let test_program = "\nfn main() {\n  let x = 5\n  let y = 10\n}"
let test_return = "\nfn main() {\n return ~(-5)\n}"
(* let test_assign = "let x = 5"*)

module ArmCG = Codegen.CodeEmitter (Codegen.Arm64)

let () =
  let prog_to_compile = test_return in
  let lexbuf = Lexing.from_string prog_to_compile in
  Printf.printf "Source: \"%s\n\" \n\n To AST =>\n\n" prog_to_compile;
  let open Parse in
  match parse_program lexbuf with
  | Ok ast ->
      print_ast ast;
      print_endline "\n To TAC =>\n";
      let tac_ast = Codegen.gen_program ast in
      Asm.ThreeAddr.print_tac tac_ast;
      print_endline "\n\n Emit ASM =>\n";
      let fn = match tac_ast with Function fn -> ArmCG.emit_function stderr fn in
      List.iter (fun i -> Asm.string_of_instr i |> print_endline) fn.instructions;
      ()
  | Error msg -> print_endline ("ERROR: \n" ^ msg)

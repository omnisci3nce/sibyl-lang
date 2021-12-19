open Paper.Lexer
open Paper.Parser
open Paper.Codegen

let run _ = ()

let run_prompt () =
  while true do
    print_string "> ";
    let input = read_line () in
    run input;
  done

let run_file filename =
  let source = filename |> read_whole_file in
  let gen = new_generator "output.s" in
  (* let t = tokenise source in List.iter print_token t;
  let ast = parse t in List.iter print_stmt ast; *)
  let asm = source |> tokenise |> parse |> codegen gen in (* tokenise -> parse -> generate assembly *)
  print_string "tokenise -> parse -> generate assembly";
  let ch = open_out "output.s" in
  print_endline " -> write to file";
  Printf.fprintf ch "%s" asm; flush ch; close_out ch; (* write assembly to file *) 
  print_string "running scripts : ";
  let _ = print_string "assembler"; Sys.command "nasm -f elf64 output.s -o output.o" in
  let _ = print_string " -> linker"; Sys.command "gcc -no-pie -nostartfiles output.o -o output.exe" in
  print_string " -> executable!\n"; flush stdout; let _ =  Sys.command "./output.exe" in
  ()

(* Run program *)
let () = match Array.length Sys.argv with
| 1 -> test_parse ()
| 2 -> 
  run_file (Array.get Sys.argv 1)
| _ -> print_endline "Usage: paper [script]"; exit 64;
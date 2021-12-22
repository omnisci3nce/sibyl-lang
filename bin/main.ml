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
  let gen = JS_Backend.new_generator "output.js" in
  let t = tokenise source in List.iter print_token t;
  let ast = parse t in List.iter print_stmt ast;
  let asm = source |> tokenise |> parse |> JS_Backend.codegen gen in (* tokenise -> parse -> generate assembly *)
  print_string "tokenise -> parse -> generate assembly";
  let ch = open_out "output.js" in
  print_endline " -> write to file";
  Printf.fprintf ch "%s" asm; flush ch; close_out ch; (* write assembly to file *) 
  print_string "running scripts : ";
  (* let _ = print_string "assembler"; Sys.command "nasm -f elf64 output.s -o output.o" in
  let _ = print_string " -> linker"; Sys.command "gcc -no-pie -nostartfiles output.o -o output.exe" in *)
  print_string " -> executable!\n"; flush stdout; let _ =  Sys.command "node ./output.js" in
  ()

(* Run program *)
let () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs.format_reporter ();
  (* Logs.info (fun m -> m ~header:"START" ?tags:None "Starting main");
  Logs.warn (fun m -> m "Hey be warned by %d." 7);
  Logs.err (fun m -> m "Hey be errored.");
  Logs.debug (fun m -> m "Would you mind to be debugged a bit ?");
  Logs.app (fun m -> m "This is for the application console or stdout."); *)
  match Array.length Sys.argv with
  | 1 -> test_gen ()
  | 2 -> 
    run_file (Array.get Sys.argv 1)
  | _ -> print_endline "Usage: paper [script]"; exit 64;
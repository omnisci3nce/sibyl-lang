open Paper.Lexer
open Paper.Parser
open Paper.Codegen
open Paper.Optimise
open Paper.Interpreter

let run _ = ()

let run_prompt () =
  while true do
    print_string "> ";
    let input = read_line () in
    run input;
  done

let run_file ~parse:only_parse filename =
  let source = filename |> read_whole_file in
  let t = tokenise source in
  let ast = parse t in List.iter print_stmt ast; print_newline ();
  if not only_parse then
    let gen = JS_Backend.new_generator "output.js" in
    let asm = source |> tokenise |> parse |> constant_fold |> JS_Backend.codegen gen in (* tokenise -> parse -> generate assembly *)
    let ch = open_out "output.js" in
    Printf.fprintf ch "%s" asm; flush ch; close_out ch; (* write assembly to file *) 
    (* let _ = print_string "assembler"; Sys.command "nasm -f elf64 output.s -o output.o" in
    let _ = print_string " -> linker"; Sys.command "gcc -no-pie -nostartfiles output.o -o output.exe" in
    print_string " -> executable!\n"; flush stdout; let _ =  Sys.command "node ./output.js" in *)
    ()
  else ()

let () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs.format_reporter ();

  let usage_msg = "usage: paper [-parse] [-lex] <file>" in
  let parse = ref false in
  let lex = ref false in
  let input_files = ref "" in
  let anon_fun filename = input_files := filename in
  let speclist = [("-parse", Arg.Set parse, "only parse AST"); ("-lex", Arg.Set lex, "only tokenise")] in
  Arg.parse speclist anon_fun usage_msg;

  if !input_files = "" then
    test_interpret ()
  else
    run_file ~parse:!parse !input_files
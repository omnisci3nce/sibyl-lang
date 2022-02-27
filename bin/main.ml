open Sibyl
open Sibyl.Lexer

let run_file ~parse:only_parse filename =
  let source = filename |> read_whole_file in
  (* let gen = Backends.Tilde.new_generator "output.js" in *)
  let ast = source
    |> Lexer.tokenise
    |> Parser.parse in
  if not only_parse then
    let _ = ast |> Backends.Tilde.codegen in
    ()
  else
    List.iter Parser.print_stmt ast

let () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs.format_reporter ();

  let usage_msg = "usage: sibyl [-parse] [-lex] <file>" in
  let parse = ref false in
  let lex = ref false in
  let input_files = ref "" in
  let anon_fun filename = input_files := filename in
  let speclist = [("-parse", Arg.Set parse, "only parse AST"); ("-lex", Arg.Set lex, "only tokenise")] in
  Arg.parse speclist anon_fun usage_msg;

  if !input_files = "" then
    Interpreter.test_interpret ()
  else
    run_file ~parse:!parse !input_files

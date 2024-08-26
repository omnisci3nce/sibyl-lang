(* open Sibyl*)

let usage_msg = "sibyl [-lex] [-parse] [-codegen] <file>"
let lex = ref false
let parse = ref false
let codegen = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("-lex", Arg.Set lex, "Run the lexer but stop before parsing");
    ("-parse", Arg.Set parse, "Run upto the parser but stop before asm generation");
    ("-codegen", Arg.Set codegen, "Run upto the assembly generation but stop before code emission");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  ()

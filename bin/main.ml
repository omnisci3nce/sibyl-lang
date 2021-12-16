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
  let tokens = tokenise source in
  List.iter print_token tokens;
  let _ast = parse tokens in
  ()

(* Run program *)
let () = match Array.length Sys.argv with
| 1 -> test_gen ()
| 2 -> 
  run_file (Array.get Sys.argv 1)
| _ -> print_endline "Usage: paper [script]"; exit 64;
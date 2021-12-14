open Paper.Lexer

(* Run program *)
let () = match Array.length Sys.argv with
| 1 -> run_prompt ()
| 2 -> 
  run_file (Array.get Sys.argv 1)
| _ -> print_endline "Usage: paper [script]"; exit 64;
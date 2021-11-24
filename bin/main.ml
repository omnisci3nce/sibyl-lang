open Paper.Lexer

(* Run program *)
let () = match Array.length Sys.argv with
| 1 -> run_prompt ()
| 2 -> 
  let _ = 
    (* print_string ; *)
    print_endline (Array.get Sys.argv 1);
    run_file (Array.get Sys.argv 1) in
  
  print_string "Hello\n"
| _ -> print_endline "Usage: paper [script]"; exit 64;
open Sibyl
open Sibyl.Codegen

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = input_line inp in
  close_in inp; r

let run_source_and_collect_output source =
  let gen = JS_Backend.new_generator "test.js" in
  let instr = source |> Lexer.tokenise |> Parser.parse |> JS_Backend.codegen gen in
  let ch = open_out "test.js" in
  Printf.fprintf ch "%s" instr; flush ch; close_out ch;
  (* let return_code =  Sys.command "node ./test.js" in *)
  let sss = run "node ./test.js" in
  sss

let fibonacci n = Printf.sprintf "
fn fib(n) {
  let a = 0
  let next1 = n - 1
  let next2 = n - 2
  if (n < 2)
    return n
  else
    return (fib(next1) + fib(next2))
}
let a = fib(%d)
print a
" n

let fibonacci_new = Printf.sprintf "
fn fib(n) {
  let a = if (n < 2) then n
          else fib(n - 1) + fib(n - 2)
  return a
}
let a = fib(12)
print a
"

let test_fibonacci () =
  let output = run_source_and_collect_output (fibonacci_new) in
  Alcotest.(check string) "sss" "144" output

let () =
  let open Alcotest in
  run "Tests" [
      "function", [ 
        (* test_case "fibonacci function" `Quick test_fibonacci; *)
     ]
  ]
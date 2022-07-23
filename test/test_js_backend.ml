open Sibyl

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = input_line inp in
  close_in inp; r

let run_source_and_collect_output source =
  let gen = Backends.JS.new_generator "test.js" in
  let instr = source |> Lexer.tokenise |> Parser.parse |> Backends.JS.codegen gen in
  let ch = open_out "test.js" in
  Printf.fprintf ch "%s" instr; flush ch; close_out ch;
  (* let return_code =  Sys.command "node ./test.js" in *)
  let sss = run "node ./test.js" in
  (instr, sss)

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
  let _, output = run_source_and_collect_output (fibonacci_new) in
  Alcotest.(check string) "sss" "144" output

let test_variable () =
  let (src, _) = run_source_and_collect_output "
  let a = 5
  print a
  " in
  let expected = "const a = 5\nconsole.log(a)\n" in
  Alcotest.(check string) "declares variable" expected src

let () =
  let open Alcotest in
  run "Tests" [
      "function", [ 
        (* test_case "fibonacci function" `Quick test_fibonacci; *)
        test_case "variable declaration" `Quick test_variable
     ]
  ]

open Cinterop

let () =
  let expr = Entry.parse "55" in
  match expr with
  | Int i -> Printf.printf "Int %d\n" i
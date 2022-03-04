open Sibyl

let typecheck_from_source s = s |> Lexer.tokenise |> Parser.parse |> Typer.typecheck

let test_let_decl_types_mismatch () =
  (* if raises then fail *)
  let run () =
    let _ = typecheck_from_source "let x: int = true\n" in
    () in
  Alcotest.(check_raises) "should raise TypeError" (Typer.TypeError "let decl type mismatch") run

let test_let_decl_types_match () =
  let _ = typecheck_from_source "let x: int = 5\n" in
  ()

let test_let_decl_type_with_binary () =
  let _ = typecheck_from_source "let x: int = 5 + 5\n" in
  let _ = typecheck_from_source "let x: int = (5 + 5)\n" in ()

let test_let_decl_type_with_ifelse () =
  let _ = typecheck_from_source "let x: int = if (false && true) then 5 else 10\n" in
  ()

let test_ifelse_type_mismatch () =
  let source = "let a:int = if (true) then 20 else false\n" in
  Alcotest.(check_raises) "should raise TypeError" (Typer.TypeError "return type of then and else branches must match") (fun _ -> let _ = typecheck_from_source source in ())


let () =
  let open Alcotest in
  run "Typer Tests" [
    "Typechecking", [
      test_case "let declaration types mismatch" `Quick test_let_decl_types_mismatch;
      test_case "let declaration types match" `Quick test_let_decl_types_match;
      test_case "let declaration types match with binary operator" `Quick test_let_decl_type_with_binary;
      test_case "let declaration types match with if/else expr" `Quick test_let_decl_type_with_ifelse;
      test_case "if else branch types match" `Quick test_ifelse_type_mismatch;
    ]
  ]

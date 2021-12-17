let lower_to_ir _s = ""
let optimise_ir _s = ""

type architecture = X86_x64 | ARM64
let generate (a: architecture) _s = match a with
  | X86_x64 -> "x"
  | ARM64 -> "a"

let compile source (arch: architecture) =
  source
  |> Lexer.tokenise
  |> Parser.parse
  |> Typer.type_tree
  |> lower_to_ir
  |> optimise_ir
  |> generate arch
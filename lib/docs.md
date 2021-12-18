# Paper Internal Documentation

## Overview

Very roughly the compiler pipeline looks like this

```ocaml
let compile source =
  source
  |> Lexer.tokenise
  |> Parser.parse
  |> Typer.type_tree
  |> Middle.lower_to_ir
  |> Optimiser.optimise_ir
  |> Codegen.generate
```
expressed in one line as  
`tokenise -> parse -> type -> lower -> optimise -> codegen`
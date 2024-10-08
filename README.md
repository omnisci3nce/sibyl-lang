# sibyl

ML-inspired programming language for learning purposes.

The goals are for Sibyl to be:

- Functional (as opposed to OOP) but not to a fault - OCaml
- Flexible and modular - Module system
- Simple - Few syntactic structures

Currently I'm learning about compilers via Crafting Interpreters and then applying what I learn to Sibyl as well as any additional information I learn from other sources such as books, Reddit, and Youtube.

Compiling on Mac and running using Rosetta

`clang -arch x86_64 example.s -o example_x86`

on mac

`dune exec bin/example.exe 2> program.s && clang -arch arm64 program.s -o program`

`./example_x86`

## Dependencies

`nasm`

## Vague Roadmap

- [x] Booleans & Ints
- [x] If Else expression (Windows x64/Interpreter/JS)
- [~30% Interp/JS only] Functions
- Loops
- String literals
- Static arrays
- Lists
- String type
- Pattern matching
- File IO
- Algebraic Data types
- Dynamic arrays / slices
- Tail recursion
- Type inference
- Tuples
- Garbage Collector
- Module system
- Structs
- Stdlib: JSON
- Stdlib: HTTP

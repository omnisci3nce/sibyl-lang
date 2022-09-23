open Codegen

module Make (CG: CodeGenerator) = struct
  include CG
  (* Backend-agnostic code generation should go here *)
end

module JS = Make (Js_backend.CodeGen)

(* * Focus on JavaScript and Tilde for now! *)
(* Code Generation Platforms:
  Transpile ->
    - JS ✔
    - OCaml (?)
    - C (?)

  Leverage existing backend ->
    - Tilde ✔
    - LLVM (?)
  
  Built-in code gen
   - x64 ✔
   - ARM ✔
   - WASM (?)

   A bytecode (?)
*)
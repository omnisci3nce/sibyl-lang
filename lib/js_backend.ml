open Printf
open Codegen
open Lexer

module CodeGen : CodeGenerator = struct
  let new_generator filename = new_generator_ filename ".js"
  let close_generator g = close_out g.channel
  let generate_begin = ""
  let generate_end = ""
  let generate_entrypoint = ""
  let generate_exit = ""
  let var _g s = s
  let gen_plus_op a b gen = 
    let name, off (* Don't need offset in JS *) = alloc_temp_var gen in
    let _ = gen
    |> emit ("const " ^ name ^ " = " ^ a ^ "+" ^ b)    
    in
    (name, off)
  let gen_sub_op a b gen = 
    let name, off (* Don't need offset in JS *) = alloc_temp_var gen in
    let _ = gen
    |> emit ("const " ^ name ^ " = " ^ a ^ "-" ^ b)    
    in
    (name, off)
  let gen_mult_op a b gen = 
    let name, off (* Don't need offset in JS *) = alloc_temp_var gen in
    let _ = gen
    |> emit ("let " ^ name ^ " = " ^ a ^ "*" ^ b)    
    in
    (name, off)
  let gen_and_op a b gen = 
    let (name, off) = alloc_temp_var gen in
    let _ = gen |> emit (sprintf "const %s = %s && %s" name a b)
    in (name, off)
  let gen_or_op a b gen = 
    let (name, off) = alloc_temp_var gen in
    let _ = gen |> emit (sprintf "const %s = %s || %s" name a b)
    in (name, off)
  let gen_not_op a gen =
    let (name, off) = alloc_temp_var gen in
    let _ = gen |> emit (sprintf "const %s = !(%s)" name a)
    in (name, off)
  let gen_print var gen = emit ("console.log(" ^ var ^ ")") gen
  let gen_copy_ident target name gen = gen |> emit ("const " ^ target ^ " = " ^ name)
  let gen_assign target str gen = 
    if Hashtbl.mem gen.variables target then emit ("const " ^ target ^ " = " ^ str) gen
    else emit (target ^ " = " ^ str) gen

  let gen_def_function name args body (gen: generator) =
    let arg_str = (List.nth args 0).lexeme in
    gen |> emit ("function " ^ name ^ "(" ^ arg_str  ^ ") {\n" ^ body ^ "\n}\n")
end

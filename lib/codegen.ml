(* Turn AST into assembly *)

open Lexer
open Parser
open Printf
let _MAX_STACK_SIZE = 1024
let temp_v_counter = ref 0

type generator = {
  variables: (string, int) Hashtbl.t;
  mutable instruction_count: int;
  mutable instructions: string;
  filepath: string;
  channel: out_channel
}

module type CodeGenerator = sig
  val new_generator : string -> generator
  val close_generator : generator -> unit
  val generate_begin : string
  val generate_end : string
  val generate_entrypoint : string
  val generate_exit : string
  val var : generator -> string -> string
  val gen_plus_op : string -> string -> generator -> (string * int)
  val gen_sub_op : string -> string -> generator -> (string * int)
  val gen_mult_op : string -> string -> generator -> (string * int)
  val gen_and_op : string -> string -> generator -> (string * int)
  val gen_or_op : string -> string -> generator -> (string * int)
  val gen_not_op : string -> generator -> (string * int)
  val gen_print : string -> generator -> generator
  val gen_copy_ident : string -> string -> generator -> generator
  val gen_assign : string -> string -> generator -> generator
  (*                     name   -> body     *)
  val gen_def_function : string -> token list -> string -> generator -> generator
end

let new_generator_ filename extension =
  let filepath = (Filename.chop_extension filename) ^ extension in
  {
    variables = Hashtbl.create 100;
    instructions = "";
    instruction_count = 0;
    filepath;
    channel = open_out filepath
  }

(* Target platform agnostic helpers *)
let emit str gen =
  gen.instruction_count <- gen.instruction_count + 1;
  gen.instructions <- gen.instructions ^ "  " ^ str ^ "\n";
  gen

let bottom_var g = Hashtbl.fold (fun _ v c -> if v >= c then (v+8) else c) g.variables 0
let empty_var g i = (bottom_var g)+(8*(i-1))

let is_alloc_var gen var_name = Hashtbl.mem gen.variables var_name
let alloc_var var_name (g: generator) = if Hashtbl.mem g.variables var_name then
                                          failwith "Var already exists!!!"
                                        else
                                          let available = empty_var g 1 in
                                          Logs.debug (fun m -> m "[Codegen] Allocating variable '%s' at offset %d" var_name available);
                                          Hashtbl.add g.variables var_name available; available

let alloc_temp_var g = 
  let var_name = ("__temp" ^ (string_of_int !temp_v_counter))  in
  if Hashtbl.mem g.variables var_name then
    failwith "Var already exists!!!"
  else let available = empty_var g 1 in
    temp_v_counter := !temp_v_counter + 1;
    Logs.debug (fun m -> m "[Codegen] Allocating temp variable '%s' at offset %d" var_name available);
    Hashtbl.add g.variables var_name available; (var_name, available)

type target = AMD64 | AARCH_64 | RISCV | JS | WASM (* Target platforms that I'd like to support *)

module X64_Backend = Backends.Make (X64_CodeGen)
module JS_Backend = Backend (JS_CodeGen)

let compile ~target filepath (_ast: statement list) =
  let gen = match target with
    | AMD64 -> X64_Backend.new_generator filepath
    | AARCH_64 -> failwith "This backend is not implemented yet"
    | RISCV -> failwith "This backend is not implemented yet"
    | JS -> JS_Backend.new_generator filepath
    | WASM -> failwith "This backend is not implemented yet"
  in
  let ch = open_out "output.js" in
  Printf.fprintf ch "%s" gen.instructions 

let test_gen () = 
  let s = "let a = (10 + 10) * 20\n" in
  let ast = s |> tokenise |> parse in
  print_endline "List: "; List.iter print_stmt ast;
  let gen = X64_Backend.new_generator "output.s" in
  printf "Num temp vars: %d\n" !temp_v_counter;
  let asm = ast |> Optimise.constant_fold |> X64_Backend.codegen gen in
  printf "Instruction count: %d\n" gen.instruction_count;
  let ch = open_out gen.filepath in
  Printf.fprintf ch "%s" asm (* write assembly to file *)
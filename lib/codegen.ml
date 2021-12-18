(* Turn AST into assembly *)

open Lexer
open Parser

let generate_begin =
"
; Generated by Paper-lang compiler
section .data
  out: db '%d', 10, 0

section .text
global _start       ; Provide program starting address to linker
"

let generate_startup =
"
extern printf

_start:
  ; Preallocate 1024 bytes on the stack to use for the whole program (for now)
  sub rsp, 1024

"

let generate_end =
"
  add rsp, 1024 ; Return stack pointer to start 
"

let generate_exit = "
  ; Load Linux exit syscall
  mov rdi, 0
  mov rax, 60
  syscall
"

type offset = int
type generator = {
  variables: (string, offset) Hashtbl.t;
  mutable instruction_count: int;
  mutable asm: string;
  filepath: string;
  channel: out_channel
}

let new_generator filename =
  let filepath = (Filename.chop_extension filename) ^ ".s" in
  {
    variables = Hashtbl.create 100;
    asm = "";
    instruction_count = 0;
    filepath;
    channel = open_out filepath
  }

let close_generator generator = close_out generator.channel

(* emit one line of asm *)
let emit str gen =
  gen.instruction_count <- gen.instruction_count + 1;
  gen.asm <- gen.asm ^ "  " ^ str ^ "\n";
  gen

  
  
let bottom_var g = Hashtbl.fold (fun _ v c -> if v >= c then (v+8) else c) g.variables 0
let empty_var g i = (bottom_var g)+(8*(i-1))

let is_alloc_var gen var_name = Hashtbl.mem gen.variables var_name
let alloc_var var_name (g: generator) = if Hashtbl.mem g.variables var_name then
                                          failwith "Var already exists!!!"
                                        else
                                          let available = empty_var g 1 in
                                          Hashtbl.add g.variables var_name available; available

let gen_plus_op a b gen =
  gen
  |> emit ("mov rax, " ^ (string_of_int a))
  |> emit ("mov rcx, " ^ (string_of_int b))
  |> emit "add rax, rcx ; output of addition is now in rax"

let gen_print var gen = 
  let offset = Hashtbl.find gen.variables var in
emit ("
  mov edi, out      ; 64-bit ABI passing order starts w/ edi, esi, ... so format string goes into the first argument
  mov esi, [rsp+ " ^ string_of_int offset ^ "]  ; arg1 goes into esi
  mov eax, 0        ; printf has varargs so eax counts num. of non-integer arguments being passed
  call printf
") gen

let gen_from_expr gen expr = match expr with
  | Binary b -> begin
    match b.operator with
    | t when t.token_type = Plus -> begin
      match b.left_expr, b.right_expr with
      | Literal (_, NumberLiteral a), Literal (_, NumberLiteral b) ->
        gen_plus_op a b gen
      | _ -> failwith "Cant add these types"
    end
    | _ -> gen
  end
  | _ -> gen

let gen_from_stmt gen (ast: statement) = match ast with
  | Expression e ->
    begin
      match e with
      | Let assignment ->
        (* Check if var has already been allocated *)
        let offset = if is_alloc_var gen assignment.identifier then
          Hashtbl.find gen.variables assignment.identifier
        else 
        (* Allocate the variable to keep track of it *)
          alloc_var assignment.identifier gen
        in
        (* Compute what we want to store in it *)
        let new_gen = gen_from_expr gen assignment.expr in
        let new_gen = emit ("mov [rsp+" ^ (string_of_int offset) ^ "], rax  ; move var \"" ^ assignment.identifier ^ "\" to offset " ^ string_of_int offset ^" on the stack\n") new_gen in
        new_gen
      | _ -> gen
    end
  | Print e -> begin
    match e with
    | Var v -> gen_print v gen
    | _ -> gen
  end

let codegen gen (ast: statement list) : string = 
  let _stmt = List.nth ast 0 in
  let rec inner gen stmts = match stmts with
    | [] -> gen
    | s :: rest ->
      let next = gen_from_stmt gen s in
      inner next rest
  in
  let final = inner gen ast in
  let output = generate_begin ^ generate_startup ^  final.asm ^ generate_end ^ generate_exit in
  output

let test_gen () = 
  let s = "let a = 10 + 10 + 10\n" in
  let _gen = new_generator "output.s" in
  let t = s |> tokenise in List.iter print_token t;
  let asm = s |> tokenise |> parse |> Optimise.optimise in (* |> codegen gen in (* tokenise -> parse -> generate assembly *) *)
  List.iter print_stmt asm
  (* print_endline asm; *)
  (* let ch = open_out "output.s" in *)
  (* Printf.fprintf ch "%s" asm write assembly to file  *)
open Ast
open Asm

let temp_id = ref 0

let next_id () =
  let i = !temp_id in
  temp_id := !temp_id + 1;
  i

let next_tmp () = "_tmp" ^ string_of_int (next_id ())
let gen_unary = function Ast.Complement -> ThreeAddr.Complement | Ast.Negate -> ThreeAddr.Negate

let rec gen_expr expr instrs =
  let open ThreeAddr in
  match expr with
  | Int i -> (ThreeAddr.Constant i, instrs)
  | UnaryOp { operator; rhs } ->
      let src, instrs = gen_expr rhs instrs in
      let dst = Var (next_tmp ()) in
      (dst, Unary { op = gen_unary operator; src; dst } :: instrs)
  | _ -> failwith "TODO"

let gen_stmt stmt =
  match stmt with
  | Return { value } ->
      let v, instrs = gen_expr value [] in
      List.rev (ThreeAddr.Return v :: instrs)
  | _ -> failwith ""

let gen_toplevel_item = function
  | Stmt _s -> failwith "brah"
  | FuncDecl { func_name; body; _ } ->
      ThreeAddr.(Function { ident = func_name; body = gen_stmt (List.nth body 0) })

let gen_program (prog : Ast.program) : ThreeAddr.program =
  List.nth prog 0 |> gen_toplevel_item (* List.map gen_toplevel_item prog *)

module type Target = sig
  (* val show_operand : Asm.operand -> string*)
  val emit_instr : out_channel -> Asm.instruction -> unit
end

let fix_function_name name = if Sys.unix then "_" ^ name else name

(* module X86_64 : Target = struct
     (* let show_operand op = match op with Asm.Imm i -> Printf.sprintf "$%d" i | Asm.Register -> "%eax"*)

     let emit_instr chan inst =
       match inst with
       | Asm.Ret -> Printf.fprintf chan "\tretq\n"
       | Asm.Mov (src, dst) ->
           Printf.fprintf chan "\tmovl %s, %s" (show_operand src) (show_operand dst)

     (* let emit_function chan (fn_def : Asm.function_def) =
        let fn_name = fix_function_name fn_def.name in
        Printf.fprintf chan "_globl %s\n%s:\n" fn_name fn_name;
        List.iter
          (fun instr ->
            emit_instr chan instr;
            print_newline ())
          fn_def.instructions*)
   end*)

module Arm64 : Target = struct
  open Printf

  let show_reg = function AX -> "x0" | R10 -> "x10"

  let show_operand op =
    match op with
    | Asm.Imm i -> sprintf "#%d" i
    | Asm.Register r -> show_reg r
    | Pseudo ident -> ident
    | Stack _i -> "TODO"

  let _emit_epilogue _inst = failwith "TODO"

  let emit_instr chan inst =
    let open Asm in
    match inst with
    | Ret -> fprintf chan "\tret\n"
    | Unary (_inst, _operand) -> ()
    | Mov (src, dst) -> fprintf chan "\t mov %s, %s" (show_operand dst) (show_operand src)
    | AllocateStack _ -> failwith ""
  (* | AsmRet -> fprintf chan "\tret\n"
     | Asm.Mov (src, dst) -> fprintf chan "\t mov %s, %s" (show_operand dst) (show_operand src)*)
end

module CodeEmitter (T : Target) = struct
  open ThreeAddr

  type env = Asm.instruction list

  let convert_value (v : ThreeAddr.value) : Asm.operand =
    match v with Constant i -> Imm i | Var v -> Pseudo v

  let convert_unary_op = function Complement -> Not | Negate -> Neg

  (** Convert Three-address code AST to Asm AST but use pseudo identifiers *)
  let convert_tac_instr env inst =
    let instructions =
      match inst with
      | Return v -> [ Asm.Mov (convert_value v, Asm.Register AX); Asm.Ret ]
      | Unary { op; src; dst } ->
          let dst_reg = convert_value dst in
          [ Asm.Mov (convert_value src, dst_reg); Asm.Unary (convert_unary_op op, dst_reg) ]
    in
    env @ instructions

  (* let emit_function chan (fn_def : function_def) =
     let fn_name = fix_function_name fn_def.ident in
     Printf.fprintf chan "_globl %s\n%s:\n" fn_name fn_name;
     List.iter
       (fun instr ->
         T.emit_instr chan instr;
         print_newline ())
       fn_def.body*)
end

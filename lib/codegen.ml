open Ast
open Asm

let next_tmp =
  let id = ref 0 in
  fun () ->
    let tmp = "_tmp" ^ string_of_int !id in
    incr id;
    tmp

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
   (* TODO: Fix x64 *)
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
    match inst with
    | Ret -> fprintf chan "\tret\n"
    | Unary (_inst, _operand) -> ()
    | Mov (src, dst) -> fprintf chan "\t mov %s, %s" (show_operand dst) (show_operand src)
    | AllocateStack _ -> failwith ""
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

  (** Replace [Pseudo] with stack pointer offsets  *)
  let pseudo_to_stack_offsets insts =
    let pseudo_map = Hashtbl.create 10 in
    let stack_size = ref 0 in
    let replace_if_pseudo operand =
      match operand with
      | Pseudo id ->
          let offset =
            match Hashtbl.find_opt pseudo_map id with
            | Some i -> i
            | None ->
                stack_size := !stack_size + 4;
                Hashtbl.add pseudo_map id !stack_size;
                !stack_size
          in
          Stack offset
      | _ -> operand
    in
    let replace_in_instruction = function
      | Mov (src, dst) -> Mov (replace_if_pseudo src, replace_if_pseudo dst)
      | Unary (u, dst) -> Unary (u, replace_if_pseudo dst)
      | other -> other
    in
    List.map replace_in_instruction insts

  let emit_function chan (fn_def : function_def) =
    let fn_name = fix_function_name fn_def.ident in
    Printf.fprintf chan "_globl %s\n%s:\n" fn_name fn_name;
    fn_def.body
    |> List.concat_map (convert_tac_instr [])
    |> pseudo_to_stack_offsets
    |> List.map (T.emit_instr chan)
end

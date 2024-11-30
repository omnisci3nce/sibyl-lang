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
  | _ -> failwith "fdd"

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

module Arm64 : Target = struct
  open Printf

  let show_reg = function ReturnReg -> "x0" | ScratchReg -> "x10"

  let show_operand op =
    match op with
    | Asm.Imm i -> sprintf "#%d" i
    | Asm.Register r -> show_reg r
    | Pseudo ident -> ident
    | Stack i -> sprintf "[sp, #%d]" i

  let _emit_prologue _inst = failwith "TODO"
  let _emit_epilogue _inst = failwith "TODO"

  let emit_instr chan inst =
    match inst with
    | Ret -> fprintf chan "\tret\n"
    | Unary (Not, _op) -> () (* fprintf chan "\tmvn %s\n" (show_operand op) *)
    | Unary (Neg, _op) -> ()
    | Mov (Register r, op) -> (* Store operation *) fprintf chan "\tstr %s, %s\n" (show_reg r) (show_operand op)
    | Mov (op, Register r) -> (* Load operation *) fprintf chan "\tldr %s, %s\n" (show_reg r) (show_operand op)
    | Mov (Stack _, Stack _) -> failwith "We cant move between two stack memory offsets"
    | Mov (Imm c, Stack i) -> 
        fprintf chan "\tmov %s, %s\n" (show_reg ScratchReg) ("#" ^ (string_of_int c));
        fprintf chan "\tstr %s, %s\n" (show_reg ScratchReg) (show_operand (Stack i))

    | Mov (o1, o2) -> printf "cant mov %s %s\n" (show_operand o1) (show_operand o2) ; failwith "Unhandled Mov type"
    | AllocateStack n -> fprintf chan "\tsub sp, sp, #%d\n" n
end

module X86_64 : Target = struct
  (* open Printf *)

   let emit_instr _chan _inst = failwith "TODO"
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
      | Return v -> [ Asm.Mov (convert_value v, Asm.Register ReturnReg); Asm.Ret ]
      | Unary { op; src; dst } ->
          let dst_reg = convert_value dst in
          [ Asm.Mov (convert_value src, dst_reg); Asm.Unary (convert_unary_op op, dst_reg) ]
    in
    env @ instructions

  (** Replace [Pseudo] operands with stack pointer offsets  *)
  let pseudo_to_stack_offsets insts =
    let pseudo_map = Hashtbl.create 10 in
    let stack_size = ref 0 in
    let replace_if_pseudo operand =
      match operand with
      | Pseudo id -> (
          match Hashtbl.find_opt pseudo_map id with
          | Some i -> Stack i
          | None ->
              stack_size := !stack_size + 4;
              Hashtbl.add pseudo_map id !stack_size;
              Stack !stack_size)
      | _ -> operand
    in
    let replace_in_instruction = function
      | Mov (src, dst) -> Mov (replace_if_pseudo src, replace_if_pseudo dst)
      | Unary (u, dst) -> Unary (u, replace_if_pseudo dst)
      | other -> other
    in
    let stackified = List.map replace_in_instruction insts in
    (stackified, !stack_size)

  let instruction_fixup (insts, stack_size) =
    let rewrite_invalid_mov inst = match inst with
    | Mov (Stack a, Stack b) -> [
      (* First move source stack addr into register, then move register into dest stack addr *)
      Mov (Stack a, Register ScratchReg);
      Mov (Register ScratchReg, Stack b)
    ]
    | other -> [other] (* Otherwise just return hte instruction *) in

    AllocateStack stack_size :: (List.concat_map rewrite_invalid_mov insts)

  let emit_function chan (fn_def : ThreeAddr.function_def) : Asm.function_def=
    let fn_name = fix_function_name fn_def.ident in
    Printf.fprintf chan ".globl %s\n%s:\n" fn_name fn_name;
    let instrs = fn_def.body
    |> List.concat_map (convert_tac_instr [])
    |> pseudo_to_stack_offsets |> instruction_fixup
    |> List.map (fun inst -> T.emit_instr chan inst; inst) 
   in
    { name = fn_name; instructions = instrs}

end

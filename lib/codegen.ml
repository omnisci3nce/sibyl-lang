open Ast

(* --- Assembly AST generation *)

let gen_expr expr : Asm.operand =
  match expr with
  | Int i -> Asm.Imm i
  | Var _ -> failwith "TODO: impl variables"
  | _ -> failwith "todo"

let gen_stmt stmt : Asm.instruction list =
  match stmt with
  | Return { value } -> Asm.[ Mov (gen_expr value, Register); Ret ]
  (* | FuncDecl { body; _ } -> [Asm.]*)
  | _ -> failwith "TODO: gen_stmt"

let gen_toplevel_item = function
  | Stmt _s -> failwith "brah"
  | FuncDecl { func_name; body; _ } ->
      Asm.{ name = func_name; instructions = List.map gen_stmt body |> List.flatten }

let gen_program (prog : Ast.program) = List.map gen_toplevel_item prog

(* --- Assembly emission *)

let show_operand op = match op with Asm.Imm i -> Printf.sprintf "$%d" i | Asm.Register -> "%eax"

let emit_instr chan inst =
  match inst with
  | Asm.Ret -> Printf.fprintf chan "\tretq\n"
  | Asm.Mov (src, dst) -> Printf.fprintf chan "\tmovl %s, %s" (show_operand src) (show_operand dst)

let fix_function_name name = if Sys.unix then "_" ^ name else name

let emit_function chan (fn_def : Asm.function_def) =
  let fn_name = fix_function_name fn_def.name in
  Printf.fprintf chan "_globl %s\n%s:\n" fn_name fn_name;
  List.iter
    (fun instr ->
      emit_instr chan instr;
      print_newline ())
    fn_def.instructions

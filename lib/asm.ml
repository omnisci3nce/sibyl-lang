type operand = Imm of int  (** Immediate *) | Register

type instruction =
  | Mov of (operand * operand)  (** Copies the first operand (src) into the second operand (dst)) *)
  | Ret

type function_def = { name : string; instructions : instruction list }

let string_of_operand = function Imm i -> "Imm " ^ string_of_int i | Register -> "Register"

let string_of_instr = function
  | Mov (src, dst) -> Printf.sprintf "Mov %s %s" (string_of_operand src) (string_of_operand dst)
  | Ret -> "Ret"

let string_of_func_def fn_def =
  Printf.sprintf "%s:\n%s" fn_def.name
    (List.map string_of_instr fn_def.instructions |> String.concat "\n")

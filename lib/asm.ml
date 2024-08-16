type operand = Imm of int  (** Immediate *) | Register

type instruction =
  | Mov of (operand * operand)  (** Copies the first operand (src) into the second operand (dst)) *)
  | Ret

type function_def = { name : string; instructions : instruction list }

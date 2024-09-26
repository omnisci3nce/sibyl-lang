module ThreeAddr = struct
  (** Three-address code  *)

  type unary_op = Complement | Negate
  type value = Constant of int | Var of string
  type instruction = Return of value | Unary of { op : unary_op; src : value; dst : value }
  type function_def = { ident : string; body : instruction list }
  type program = Function of function_def

  let string_of_value = function Constant i -> "Constant " ^ string_of_int i | Var v -> "Var " ^ v

  let string_of_instruction = function
    | Return v -> "Return " ^ string_of_value v
    | Unary { op = _; src; dst } ->
        Printf.sprintf "Unary (op, %s %s)" (string_of_value src) (string_of_value dst)

  let print_tac ast =
    match ast with
    | Function f ->
        Printf.printf "Function '%s'\n%s" f.ident
          (List.map string_of_instruction f.body |> String.concat "\n")
end

type identifier = string
type reg = AX | R10
type unary_inst = Neg | Not

type operand =
  | Imm of int  (** Immediate *)
  | Register of reg
  | Pseudo of identifier
  | Stack of int

type instruction =
  | Mov of (operand * operand)  (** Copies the first operand (src) into the second operand (dst)) *)
  | Unary of (unary_inst * operand)
  | AllocateStack of int
  | Ret

type function_def = { name : string; instructions : instruction list }

let string_of_operand = function
  | Imm i -> "Imm " ^ string_of_int i
  | Register _ -> "Register"
  | Pseudo id -> "Pseudo " ^ id
  | Stack n -> "Stack " ^ string_of_int n ^ "bytes"

let string_of_unary = function Neg -> "Neg" | Not -> "Not"

let string_of_instr = function
  | Mov (src, dst) -> Printf.sprintf "Mov %s -> %s" (string_of_operand src) (string_of_operand dst)
  | Unary (u, operand) -> string_of_unary u ^ " " ^ string_of_operand operand
  | AllocateStack _ -> ""
  | Ret -> "Ret"

let string_of_func_def fn_def =
  Printf.sprintf "%s:\n%s" fn_def.name
    (List.map string_of_instr fn_def.instructions |> String.concat "\n")

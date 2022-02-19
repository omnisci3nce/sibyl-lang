open Ctypes
open Foreign

type type_t = Void | Bool | I8 | I16 | I32 | I64 | F32 | F64 | PTR
let uint_of_type = function
| Void -> Unsigned.UInt8.of_int 0
| Bool -> Unsigned.UInt8.of_int 1
| I8 -> Unsigned.UInt8.of_int 2
| I16 -> Unsigned.UInt8.of_int 3
| I32 -> Unsigned.UInt8.of_int 4
| I64 -> Unsigned.UInt8.of_int 5
| F32 -> Unsigned.UInt8.of_int 6
| F64 -> Unsigned.UInt8.of_int 7
| PTR -> Unsigned.UInt8.of_int 8

type arithmatic_behaviour_t =
  | AssumeNSW
  | AssumeNUW
  | CanWrap
  | SignedTrapOnWrap
  | UnsignedTrapOnWrap
  | SaturatedUnsigned
  | SaturatedSigned

let int_of_arithmatic_behaviour = function
  | AssumeNSW -> 0
  | AssumeNUW -> 1
  | CanWrap -> 2
  | SignedTrapOnWrap -> 3
  | UnsignedTrapOnWrap -> 4
  | SaturatedUnsigned -> 5
  | SaturatedSigned -> 6

type branch_hint_t =
  | BranchHintNone
  | BranchHintLikely
  | BranchHintUnlikely

type linkage_t =
  | Public
  | Private

module Architecture = struct
  type t = X86_64 | AARCH64

  let to_int = function
    | X86_64 -> 0
    | AARCH64 -> 1
  let of_int = function
    | 0 -> X86_64
    | 1 -> AARCH64
    | _ -> failwith "unknown architecture."
end

module TargetSystem = struct
  type t =
    | Windows
    | Linux
    | MacOS
    | Android

  let to_int = function
    | Windows -> 0
    | Linux -> 1
    | MacOS -> 2
    | Android -> 3
  let of_int = function
    | 0 -> Windows
    | 1 -> Linux
    | 2 -> MacOS
    | 3 -> Android
    | _ -> failwith "unknown target system."
end

module CallingConv = struct
  type t =
    | CDecl
    | StdCall
  let to_int = function
    | CDecl -> 0
    | StdCall -> 1
end

type module_t
let tb_module : module_t structure typ = structure "TB_Module"

type function_t
let tb_function : function_t structure typ = structure "TB_Function"

type function_prototype_t
let tb_function_prototype : function_prototype_t structure typ = structure "TB_FunctionPrototype"

type x64_struct_t
let x64_struct : x64_struct_t structure typ = structure "x64"
let sse3 = field x64_struct "sse3" bool
let popcnt = field x64_struct "popcnt" bool
let lzcnt = field x64_struct "lzcnt" bool
let sse41 = field x64_struct "sse41" bool
let sse42 = field x64_struct "sse42" bool
let clmul = field x64_struct "clmul" bool
let f16c = field x64_struct "f16c" bool
let bmi1 = field x64_struct "bmi1" bool
let bmi2 = field x64_struct "bmi2" bool
let avx = field x64_struct "avx" bool
let avx2 = field x64_struct "avx2" bool
let () = seal x64_struct

type aarch64_struct_t
let aarch64_struct : aarch64_struct_t structure typ = structure "aarch64"
let bf16 = field aarch64_struct "bf16" bool
let () = seal aarch64_struct


type feature_set_t
let feature_set : feature_set_t structure typ = structure "TB_FeatureSet"
let x64 = field feature_set "x64" x64_struct
let aarch64 = field feature_set "aarch64" aarch64_struct
let () = seal feature_set

type datatype_t
let tb_datatype : datatype_t structure typ = structure "TB_DataType"
let ttype = field tb_datatype "type" uint8_t
let width = field tb_datatype "width" uint8_t
let () = seal tb_datatype

(*
type register_t
let tb_register : register_t structure typ = structure "TB_Register"
*)

let tb_feature_set_zero = foreign "tb_feature_set_zero"
  (void @-> returning feature_set)
  
let module_create = foreign "tb_module_create"
  (int @-> int @-> ptr feature_set @-> returning (ptr tb_module))
  
let module_compile = foreign "tb_module_compile"
  (ptr tb_module @-> returning bool)

(* type file_t
let file_ptr : file_t structure typ = structure "FILE" *)

(* let fopen = foreign "fopen"
  (string @-> string @-> returning (ptr file_ptr)) *)

let module_export = foreign "tb_module_export"
  (ptr tb_module @-> string @-> bool @-> returning bool)

let function_create = foreign "tb_prototype_create"
  (ptr tb_module @-> int @-> tb_datatype @-> int @-> bool @-> returning (ptr tb_function_prototype))

let function_add_param = foreign "tb_prototype_add_param"
  (ptr tb_function_prototype @-> ptr tb_datatype @-> returning void)

let function_build = foreign "tb_prototype_build"
  (ptr tb_module @-> ptr tb_function_prototype @-> string @-> int @-> returning (ptr tb_function))

let function_compile = foreign "tb_module_compile_func"
  (ptr tb_module @-> ptr tb_function @-> returning bool)

(* let tb_function_print = foreign "tb_function_print"
  (ptr tb_function @-> returning void) *)

(* let tb_inst_param_addr = foreign "tb_inst_param_addr"
  (ptr tb_function @-> int @-> returning int) *)

(* let tb_inst_load = foreign "tb_inst_load"
  (ptr tb_function @-> tb_datatype @-> tb_register @->  *)

  (* TB_API TB_Reg tb_inst_local(TB_Function* f, uint32_t size, TB_CharUnits align);
	TB_API TB_Reg tb_inst_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits align);
	TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits align); *)
let tb_inst_local = foreign "tb_inst_local"
  (ptr tb_function @-> int @-> int @-> returning int)

let tb_inst_load = foreign "tb_inst_load"
  (ptr tb_function @-> tb_datatype @-> int @-> int @-> returning int)

let tb_inst_store = foreign "tb_inst_store"
  (ptr tb_function @-> tb_datatype @-> int @-> int @-> int @-> returning void)

let tb_inst_sint = foreign "tb_inst_sint"
  (ptr tb_function @-> tb_datatype @-> Ctypes.int64_t @-> returning int)

let tb_inst_add = foreign "tb_inst_add"
  (ptr tb_function @-> int @-> int @-> int @-> returning int)

let tb_inst_ret = foreign "tb_inst_ret"
  (ptr tb_function @-> int @-> returning void)

module Register = struct
  type t
end

(* OCaml-ify it *)
module DataType = struct
  let create value_type (width_ : int) = 
    let dt = make tb_datatype in
    setf dt ttype (uint_of_type value_type);
    setf dt width (Unsigned.UInt8.of_int width_);
    dt
end
module Instructions = struct
  (* let inst_iconst fp dt number : int = tb_inst_iconst fp dt number *)
  let inst_add fp a b arith_behav = tb_inst_add fp a b (int_of_arithmatic_behaviour arith_behav)
  let inst_return fp reg = tb_inst_ret fp reg
end

module Module = struct
  (* type t = tb_module *)
  let create arch target_system = 
    let fs_ptr = addr (tb_feature_set_zero ()) in (* initialise a zero struct and get address *)
    module_create (Architecture.to_int arch) (TargetSystem.to_int target_system) fs_ptr

end
module Function = struct
  let create m name = function_create m name
end

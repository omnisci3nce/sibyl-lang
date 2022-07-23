open Codegen

module CodeGen : CodeGenerator = struct
  let new_generator filename = new_generator_ filename ""
  let close_generator g = close_out g.channel
  let generate_begin = failwith "Unimplemented"
  let generate_end = failwith "Unimplemented"
  let generate_entrypoint = failwith "Unimplemented"
  let generate_exit = failwith "Unimplemented"
  let var _g _s = failwith "Unimplemented"
  let gen_plus_op _a _b _gen = failwith "Unimplemented"
  let gen_sub_op _a _b _gen = failwith "Unimplemented"
  let gen_mult_op _a _b _gen = failwith "Unimplemented"
  let gen_and_op _a _b _gen = failwith "Unimplemented"
  let gen_or_op _a _b _gen = failwith "Unimplemented"
  let gen_not_op _a _gen = failwith "Unimplemented"
  let gen_print _var _gen = failwith "Unimplemented"
  let gen_copy_ident _target _name _gen = failwith "Unimplemented"
  let gen_assign _target _str _gen = failwith "Unimplemented"
  let gen_def_function _name _args _body _gen = failwith "Unimplemented"
end

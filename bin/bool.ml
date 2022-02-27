open Sibyl.Tilde

let () =
  let g_module = Module.create Architecture.X86_64 TargetSystem.Windows in
  let init_proto = Function.create g_module  I64 0 in
  let init_func = Function.build g_module init_proto "init" in
  let printf_handle = tb_extern_create g_module "printf" in
  let proto = Function.create g_module I16 0 in
  let fp = Function.build g_module proto "testbools" in
  (* let local = tb_inst_local fp 4 4 in *)
  let r1 = tb_inst_sint fp DataType.i16_dt (Signed.Int64.of_int 1) in
  (* let _ = tb_inst_store fp DataType.i32_dt local r1 1 in *)
  
  let format_string = tb_inst_cstring fp "result %d\n\n" in
  (* let register = tb_inst_load fp DataType.i8_dt local 4 in *)
  let arr = make_params_array [format_string; r1] in
  let _ = tb_inst_ecall fp DataType.void_dt printf_handle 2 (Ctypes.CArray.start arr) in
  
  (* let _ = tb_inst_ret fp (tb_inst_load fp DataType.i8_dt local 4) in *)
  let _ = tb_inst_ret fp r1 in
  let _ = function_compile g_module fp in
  
  let arr = Ctypes.CArray.make Ctypes.int 0 in
  let _ = tb_inst_call init_func (DataType.i16_dt) fp 0 (Ctypes.CArray.start arr) in


  (* Return exit code from main function *)
  let return_code = Inst.i64 init_func 0 in
  Inst.return init_func return_code;
  let _ = function_compile g_module init_func in
  let _ = module_compile g_module in
  let _ = module_export g_module "./test_x64.obj" true in
  ()
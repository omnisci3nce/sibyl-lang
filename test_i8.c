#include <stdio.h>
#include <stdint.h>
#include "tb.h"

int main(int argc, char *argv[]) {
  TB_FeatureSet features = { 0 };
  TB_Module* m = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, &features);
  TB_ExternalID printf_handle = tb_extern_create(m, "printf");

  TB_FunctionPrototype* p = tb_prototype_create(m, TB_STDCALL, TB_TYPE_I8, 0, false);
	TB_Function* func = tb_prototype_build(m, p, "test", TB_LINKAGE_PUBLIC);
	
  TB_Register number = tb_inst_sint(func, TB_TYPE_I8, 1);
  TB_Register cast = tb_inst_zxt(func, number, TB_TYPE_I32);
  
  TB_Register format_string = tb_inst_cstring(func, "result %d\n");
  tb_inst_ecall(func, TB_TYPE_VOID, printf_handle, 2, (TB_Register[]) { format_string, cast });


	tb_inst_ret(func, number);

  tb_module_compile_func(m, func);
  tb_module_compile(m);
  tb_module_export(m, "./test_x64.obj", true);
}
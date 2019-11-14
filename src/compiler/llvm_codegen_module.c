// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"



static inline LLVMTypeRef gencontext_create_basic_llvm_type(GenContext *context, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_BOOL:
			return LLVMInt1TypeInContext(context->context);
		case TYPE_I8:
		case TYPE_U8:
			return LLVMInt8TypeInContext(context->context);
		case TYPE_I16:
		case TYPE_U16:
			return LLVMInt16TypeInContext(context->context);
		case TYPE_I32:
		case TYPE_U32:
			return LLVMInt32TypeInContext(context->context);
		case TYPE_I64:
		case TYPE_U64:
			return LLVMInt64TypeInContext(context->context);
		case TYPE_F32:
			return LLVMFloatTypeInContext(context->context);
		case TYPE_F64:
			return LLVMDoubleTypeInContext(context->context);
		case TYPE_VOID:
			return LLVMVoidTypeInContext(context->context);
		default:
			UNREACHABLE
	}
}

static inline void gencontext_init_basic_llvm_type(GenContext *context, Type *type)
{
	type->backend_type = gencontext_create_basic_llvm_type(context, type);
	if (context->debug.builder)
	{
		type->backend_debug_type = gencontext_create_builtin_debug_type(context, type);
	}
}
void gencontext_begin_module(GenContext *context)
{
	assert(!context->module && "Expected no module");
	const char *full_path = context->ast_context->file->full_path;
	char *mangled_module_name = strformat("module:%s", context->ast_context->module->name);
	context->module = LLVMModuleCreateWithNameInContext(mangled_module_name, context->context);
	LLVMSetModuleDataLayout(context->module, target_data_layout());
	LLVMSetSourceFileName(context->module, full_path, strlen(context->ast_context->file->full_path));

	LLVMSetTarget(context->module, build_options.target);
	if (build_options.debug_info)
	{
		const char *filename = context->ast_context->file->name;
		const char *dir_path = context->ast_context->file->dir_path;
		context->debug.builder = LLVMCreateDIBuilder(context->module);
		context->debug.file = LLVMDIBuilderCreateFile(context->debug.builder, filename, strlen(filename), dir_path, strlen(dir_path));

		bool is_optimized = false;
		const char *dwarf_flags = "";
		unsigned runtime_version = 1;
		LLVMDWARFEmissionKind emission_kind = LLVMDWARFEmissionFull;
		context->debug.compile_unit = LLVMDIBuilderCreateCompileUnit(context->debug.builder, LLVMDWARFSourceLanguageC,
		                                                             context->debug.file, DWARF_PRODUCER_NAME,
		                                                             strlen(DWARF_PRODUCER_NAME), is_optimized,
		                                                             dwarf_flags, strlen(dwarf_flags),
		                                                             runtime_version, "" /* split name */, 0 /* len */,
		                                                             emission_kind, /* dwo */0, /* inlining */0,
		                                                             /* debug for profiling */0);
	}
	// Setup all types. Not thread-safe, but at this point in time we can assume a single context.
	// We need to remove the context from the cache after this.
	// This would seem to indicate that we should change Type / actual type.
	gencontext_init_basic_llvm_type(context, type_char);
	gencontext_init_basic_llvm_type(context, type_byte);
	gencontext_init_basic_llvm_type(context, type_short);
	gencontext_init_basic_llvm_type(context, type_ushort);
	gencontext_init_basic_llvm_type(context, type_int);
	gencontext_init_basic_llvm_type(context, type_uint);
	gencontext_init_basic_llvm_type(context, type_long);
	gencontext_init_basic_llvm_type(context, type_ulong);
	gencontext_init_basic_llvm_type(context, type_float);
	gencontext_init_basic_llvm_type(context, type_double);
	gencontext_init_basic_llvm_type(context, type_void);
	gencontext_init_basic_llvm_type(context, type_bool);

	context->pointer_alignment = LLVMPointerSizeForAS(target_data_layout(), 0);

	context->block_global_unique_count = 0;
	context->ast_alloca_addr_space = target_alloca_addr_space();

/*
	SizeSizeInBytes =
			C.toCharUnitsFromBits(C.getTargetInfo().getMaxPointerWidth()).getQuantity();
	IntAlignInBytes =
			C.toCharUnitsFromBits(C.getTargetInfo().getIntAlign()).getQuantity();
	IntTy = llvm::IntegerType::get(LLVMContext, C.getTargetInfo().getIntWidth());
	IntPtrTy = llvm::IntegerType::get(LLVMContext,
	                                  C.getTargetInfo().getMaxPointerWidth());
	Int8PtrTy = Int8Ty->getPointerTo(0);
	Int8PtrPtrTy = Int8PtrTy->getPointerTo(0);
	AllocaInt8PtrTy = Int8Ty->getPointerTo(
			M.getDataLayout().getAllocaAddrSpace());
	ASTAllocaAddressSpace = getTargetCodeGenInfo().getASTAllocaAddressSpace();
*/

}


void gencontext_end_module(GenContext *context)
{
	if (context->debug.builder)
	{
		LLVMDIBuilderFinalize(context->debug.builder);
	}
	LLVMDisposeModule(context->module);
}
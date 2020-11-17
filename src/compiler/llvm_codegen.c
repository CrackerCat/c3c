// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


static void diagnostics_handler(LLVMDiagnosticInfoRef ref, void *context)
{
	char *message = LLVMGetDiagInfoDescription(ref);
	LLVMDiagnosticSeverity severity = LLVMGetDiagInfoSeverity(ref);
	const char *severerity_name = "unknown";
	switch (severity)
	{
		case LLVMDSError:
			error_exit("LLVM error generating code for %s: %s", ((GenContext *)context)->ast_context->module->name, message);
			break;
		case LLVMDSWarning:
			severerity_name = "warning";
			break;
		case LLVMDSRemark:
			severerity_name = "remark";
			break;
		case LLVMDSNote:
			severerity_name = "note";
			break;
	}
	DEBUG_LOG("LLVM message [%s]: %s ", severerity_name, message);
	LLVMDisposeMessage(message);
}

static void gencontext_init(GenContext *context, Context *ast_context)
{
	memset(context, 0, sizeof(GenContext));
	context->context = LLVMContextCreate();
	context->bool_type = LLVMInt1TypeInContext(context->context);
	context->byte_type = LLVMInt8TypeInContext(context->context);
	LLVMContextSetDiagnosticHandler(context->context, &diagnostics_handler, context);
	context->ast_context = ast_context;
}

static void gencontext_destroy(GenContext *context)
{
	LLVMContextDispose(context->context);
}

LLVMValueRef gencontext_emit_memclear_size_align(GenContext *context, LLVMValueRef ref, uint64_t size, unsigned int align, bool bitcast)
{
	LLVMValueRef target = bitcast ? LLVMBuildBitCast(context->builder, ref, llvm_type(type_get_ptr(type_byte)), "") : ref;
	return LLVMBuildMemSet(context->builder, target, LLVMConstInt(llvm_type(type_byte), 0, false),
	                LLVMConstInt(llvm_type(type_ulong), size, false), align);

}

LLVMValueRef gencontext_emit_memclear(GenContext *context, LLVMValueRef ref, Type *type)
{
	// TODO avoid bitcast on those that do not need them.
	return gencontext_emit_memclear_size_align(context, ref, type_size(type),
			type_abi_alignment(type), true);
}


static void gencontext_emit_global_variable_definition(GenContext *context, Decl *decl)
{
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);

	// Skip real constants.
	if (!decl->type) return;

	// TODO fix name
	decl->backend_ref = LLVMAddGlobal(context->module, llvm_type(decl->type), decl->name);

	if (decl->var.init_expr)
	{
		LLVMSetInitializer(decl->backend_ref, gencontext_emit_expr(context, decl->var.init_expr));
	}
	else
	{
		LLVMSetInitializer(decl->backend_ref, LLVMConstNull(llvm_type(decl->type)));
	}

	LLVMSetGlobalConstant(decl->backend_ref, decl->var.kind == VARDECL_CONST);

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
			LLVMSetVisibility(decl->backend_ref, LLVMProtectedVisibility);
			break;
		case VISIBLE_PUBLIC:
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
		case VISIBLE_LOCAL:
			LLVMSetVisibility(decl->backend_ref, LLVMHiddenVisibility);
			break;
	}

	int alignment = 64; // TODO
	// Should we set linkage here?
	if (context->debug.builder && false)
	{
		decl->var.backend_debug_ref = LLVMDIBuilderCreateGlobalVariableExpression(context->debug.builder,
		                                                                          NULL /*scope*/,
		                                                                          decl->name,
		                                                                          1, /*source_range_len(decl->name_span),*/
		                                                                          "linkagename",
		                                                                          2,
		                                                                          context->debug.file,
		                                                                          12 /* lineno */,
		                                                                          llvm_debug_type(decl->type),
		                                                                          decl->visibility ==
		                                                                          VISIBLE_LOCAL, /* expr */
		                                                                          NULL, /** declaration **/
		                                                                          NULL,
		                                                                          alignment);
	}
}
static void gencontext_verify_ir(GenContext *context)
{
	char *error = NULL;
	assert(context->module);
	if (LLVMVerifyModule(context->module, LLVMPrintMessageAction, &error))
	{
		if (*error)
		{
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}

void gencontext_emit_object_file(GenContext *context)
{
	char *err = "";
	LLVMSetTarget(context->module, build_options.target);
	char *layout = LLVMCopyStringRepOfTargetData(target_data_layout());
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	// Generate .o or .obj file
	char *filename = strformat("%.*s.o", (int)strlen(context->ast_context->file->name) - 3, context->ast_context->file->name);
	if (LLVMTargetMachineEmitToFile(target_machine(), context->module, filename, LLVMObjectFile, &err))
	{
		error_exit("Could not emit object file: %s", err);
	}
}

void gencontext_print_llvm_ir(GenContext *context)
{
	char *err = NULL;
	char *filename = strformat("%.*s.ll", (int)strlen(context->ast_context->file->name) - 3, context->ast_context->file->name);
	if (LLVMPrintModuleToFile(context->module, filename, &err))
	{
		error_exit("Could not emit ir to file: %s", err);
	}
}


LLVMValueRef gencontext_emit_alloca(GenContext *context, LLVMTypeRef type, unsigned alignment, const char *name)
{
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(context->builder);
	LLVMPositionBuilderBefore(context->builder, context->alloca_point);
	LLVMValueRef alloca = LLVMBuildAlloca(context->builder, type, name);
	LLVMSetAlignment(alloca, alignment ?: llvm_abi_alignment(type));
	LLVMPositionBuilderAtEnd(context->builder, current_block);
	return alloca;
}

LLVMValueRef gencontext_emit_alloca_aligned(GenContext *context, Type *type, const char *name)
{
	return gencontext_emit_alloca(context, llvm_type(type), type_alloca_alignment(type), name);
}

LLVMValueRef gencontext_emit_decl_alloca(GenContext *context, Decl *decl)
{
	LLVMTypeRef type = llvm_type(decl->type);
	return gencontext_emit_alloca(context,
							   type,
							   decl->alignment ?: type_alloca_alignment(decl->type),
							   decl->name ?: "anon");
}

/**
 * Values here taken from LLVM.
 * @return return the inlining threshold given the build options.
 */
static int get_inlining_threshold(void)
{
	if (build_options.optimization_level == OPTIMIZATION_AGGRESSIVE)
	{
		return 250;
	}
	switch (build_options.size_optimization_level)
	{
		case SIZE_OPTIMIZATION_TINY:
			return 5;
		case SIZE_OPTIMIZATION_SMALL:
			return 50;
		default:
			return 250;
	}
}


static inline unsigned lookup_intrinsic(const char *name)
{
	return LLVMLookupIntrinsicID(name, strlen(name));
}

static inline unsigned lookup_attribute(const char *name)
{
	return LLVMGetEnumAttributeKindForName(name, strlen(name));
}

static bool intrinsics_setup = false;
unsigned ssub_overflow_intrinsic_id;
unsigned usub_overflow_intrinsic_id;
unsigned sadd_overflow_intrinsic_id;
unsigned uadd_overflow_intrinsic_id;
unsigned smul_overflow_intrinsic_id;
unsigned umul_overflow_intrinsic_id;
unsigned trap_intrinsic_id;
unsigned assume_intrinsic_id;

unsigned noinline_attribute;
unsigned alwaysinline_attribute;
unsigned inlinehint_attribute;
unsigned noreturn_attribute;
unsigned nounwind_attribute;
unsigned writeonly_attribute;
unsigned readonly_attribute;
unsigned optnone_attribute;
unsigned align_attribute;
unsigned noalias_attribute;
unsigned sret_attribute;
unsigned zext_attribute;
unsigned sext_attribute;
unsigned byval_attribute;
unsigned inreg_attribute;

void llvm_codegen_setup()
{
	assert(intrinsics_setup == false);
	ssub_overflow_intrinsic_id = lookup_intrinsic("llvm.ssub.with.overflow");
	usub_overflow_intrinsic_id = lookup_intrinsic("llvm.usub.with.overflow");
	sadd_overflow_intrinsic_id = lookup_intrinsic("llvm.sadd.with.overflow");
	uadd_overflow_intrinsic_id = lookup_intrinsic("llvm.uadd.with.overflow");
	smul_overflow_intrinsic_id = lookup_intrinsic("llvm.smul.with.overflow");
	umul_overflow_intrinsic_id = lookup_intrinsic("llvm.umul.with.overflow");
	trap_intrinsic_id = lookup_intrinsic("llvm.trap");
	assume_intrinsic_id = lookup_intrinsic("llvm.assume");

	noinline_attribute = lookup_attribute("noinline");
	alwaysinline_attribute = lookup_attribute("alwaysinline");
	inlinehint_attribute = lookup_attribute("inlinehint");
	noreturn_attribute = lookup_attribute("noreturn");
	nounwind_attribute = lookup_attribute("nounwind");
	writeonly_attribute = lookup_attribute("writeonly");
	readonly_attribute = lookup_attribute("readonly");
	optnone_attribute = lookup_attribute("optnone");
	sret_attribute = lookup_attribute("sret");
	noalias_attribute = lookup_attribute("noalias");
	zext_attribute = lookup_attribute("zeroext");
	sext_attribute = lookup_attribute("signext");
	align_attribute = lookup_attribute("align");
	byval_attribute = lookup_attribute("byval");
	inreg_attribute = lookup_attribute("inreg");
	intrinsics_setup = true;
}

void gencontext_emit_introspection_type(GenContext *context, Decl *decl)
{
	llvm_type(decl->type);
	if (decl_is_struct_type(decl))
	{
		Decl **decls = decl->strukt.members;
		VECEACH(decls, i)
		{
			Decl *member_decl = decls[i];
			if (decl_is_struct_type(member_decl))
			{
				gencontext_emit_introspection_type(context, member_decl);
			}
		}
	}
	LLVMValueRef global_name = LLVMAddGlobal(context->module, llvm_type(type_byte), decl->name ? decl->name : "anon");
	LLVMSetGlobalConstant(global_name, 1);
	LLVMSetInitializer(global_name, LLVMConstInt(llvm_type(type_byte), 1, false));
	decl->type->backend_typeid = LLVMBuildPtrToInt(context->builder, global_name, llvm_type(type_typeid), "");

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
		case VISIBLE_PUBLIC:
			LLVMSetLinkage(global_name, LLVMLinkOnceODRLinkage);
			LLVMSetVisibility(global_name, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
		case VISIBLE_LOCAL:
			LLVMSetVisibility(global_name, LLVMHiddenVisibility);
			LLVMSetLinkage(global_name, LLVMLinkerPrivateLinkage);
			break;
	}
}

static inline uint32_t upper_power_of_two(uint32_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;
	v++;
	return v;
}

void bevalue_from_boolean(BEValue *value, LLVMValueRef llvm_value)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type_bool);
	value->kind = BE_BOOLEAN;
	value->type = type_bool;
}

void bevalue_from_value(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;
}

void bevalue_from_address(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_ADDRESS;
	value->type = type;
}

void bevalue_to_store(GenContext *context, BEValue *value)
{
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = LLVMBuildZExt(context->builder, value->value, context->byte_type, "");
	}
	value->kind = BE_VALUE;
}

void bevalue_to_value(GenContext *context, BEValue *value)
{
	if (value->kind != BE_ADDRESS) return;
	value->value = gencontext_emit_load_aligned(context, llvm_type(value->type), value->value, value->alignment ?: type_abi_alignment(value->type), "");
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = LLVMBuildTrunc(context->builder, value->value, context->bool_type, "");
		value->kind = BE_BOOLEAN;
		return;
	}
	value->kind = BE_VALUE;
}


static void gencontext_emit_decl(GenContext *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			UNREACHABLE;
		case DECL_FUNC:
			// TODO
			break;
		case DECL_VAR:
			// TODO
			break;
		case DECL_TYPEDEF:
			break;
		case DECL_ENUM_CONSTANT:
			// TODO
			break;;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERR:
			gencontext_emit_introspection_type(context, decl);
			break;
		case DECL_ENUM:
			// TODO
			break;
		case NON_TYPE_DECLS:
			UNREACHABLE
	}
}

void llvm_codegen(Context *context)
{
	assert(intrinsics_setup);
	GenContext gen_context;
	gencontext_init(&gen_context, context);
	gencontext_begin_module(&gen_context);
	// EmitDeferred()
	VECEACH(context->external_symbol_list, i)
	{
		gencontext_emit_extern_decl(&gen_context, context->external_symbol_list[i]);
	}
	VECEACH(context->methods, i)
	{
		gencontext_emit_function_decl(&gen_context, context->methods[i]);
	}
	VECEACH(context->functions, i)
	{
		gencontext_emit_function_decl(&gen_context, context->functions[i]);
	}
	VECEACH(context->types, i)
	{
		gencontext_emit_decl(&gen_context, context->types[i]);
	}
	VECEACH(context->vars, i)
	{
		gencontext_emit_global_variable_definition(&gen_context, context->vars[i]);
	}
	VECEACH(context->functions, i)
	{
		Decl *decl = context->functions[i];
		if (decl->func.body) gencontext_emit_function_body(&gen_context, decl);
	}
	VECEACH(context->methods, i)
	{
		Decl *decl = context->methods[i];
		if (decl->func.body) gencontext_emit_function_body(&gen_context, decl);
	}

	if (gen_context.debug.builder) LLVMDIBuilderFinalize(gen_context.debug.builder);
	gencontext_print_llvm_ir(&gen_context);

	// Starting from here we could potentially thread this:
	LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
	LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, build_options.optimization_level);
	LLVMPassManagerBuilderSetSizeLevel(pass_manager_builder, build_options.size_optimization_level);
	LLVMPassManagerBuilderSetDisableUnrollLoops(pass_manager_builder, build_options.optimization_level == OPTIMIZATION_NONE);
	LLVMPassManagerBuilderUseInlinerWithThreshold(pass_manager_builder, 0); //get_inlining_threshold());
	LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
	LLVMPassManagerRef function_pass_manager = LLVMCreateFunctionPassManagerForModule(gen_context.module);
	LLVMAddAnalysisPasses(target_machine(), function_pass_manager);
	LLVMAddAnalysisPasses(target_machine(), pass_manager);
	LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
	// We insert a memcpy pass here, or it will be used too late to fix our aggregate copies.
	if (!build_options.feature.no_memcpy_pass)
	{
		LLVMAddMemCpyOptPass(function_pass_manager);
	}
	LLVMPassManagerBuilderPopulateFunctionPassManager(pass_manager_builder, function_pass_manager);

	// IMPROVE
	// In LLVM Opt, LoopVectorize and SLPVectorize settings are part of the PassManagerBuilder
	// Anything else we need to manually add?

	LLVMPassManagerBuilderDispose(pass_manager_builder);

	// Run function passes
	LLVMInitializeFunctionPassManager(function_pass_manager);
	LLVMValueRef current_function = LLVMGetFirstFunction(gen_context.module);
	while (current_function)
	{
		LLVMRunFunctionPassManager(function_pass_manager, current_function);
		current_function = LLVMGetNextFunction(current_function);
	}
	LLVMFinalizeFunctionPassManager(function_pass_manager);
	LLVMDisposePassManager(function_pass_manager);

	// Run module pass
	LLVMRunPassManager(pass_manager, gen_context.module);
	LLVMDisposePassManager(pass_manager);

	// Serialize the LLVM IR, if requested
	if (build_options.emit_llvm) gencontext_print_llvm_ir(&gen_context);

	gencontext_verify_ir(&gen_context);

	if (build_options.emit_bitcode) gencontext_emit_object_file(&gen_context);

	gencontext_end_module(&gen_context);
	gencontext_destroy(&gen_context);
}

void gencontext_add_int_attribute(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, uint64_t value, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateEnumAttribute(context->context, attribute_id, value);
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, index, llvm_attr);
}

void gencontext_add_attribute(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index)
{
	gencontext_add_int_attribute(context, value_to_add_attribute_to, attribute_id, 0, index);
}

void gencontext_add_attribute_range(GenContext *context, LLVMValueRef value_to_add_attribute_to, unsigned attribute_id, int index_start, int index_end)
{
	for (int i = index_start; i <= index_end; i++)
	{
		gencontext_add_int_attribute(context, value_to_add_attribute_to, attribute_id, 0, i);
	}
}

void gencontext_add_string_attribute(GenContext *context, LLVMValueRef value_to_add_attribute_to, const char *attribute, const char *value, int index)
{
	LLVMAttributeRef llvm_attr = LLVMCreateStringAttribute(context->context, attribute, strlen(attribute), value, strlen(value));
	LLVMAddAttributeAtIndex(value_to_add_attribute_to, index, llvm_attr);
}




unsigned llvm_abi_size(LLVMTypeRef type)
{
	return LLVMABISizeOfType(target_data_layout(), type);
}

unsigned llvm_abi_alignment(LLVMTypeRef type)
{
	return LLVMABIAlignmentOfType(target_data_layout(), type);
}

void llvm_store_self_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type)
{
	llvm_store_aligned(context, pointer, value, type_abi_alignment(type));
}

void llvm_store_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, unsigned alignment)
{
	LLVMValueRef ref = LLVMBuildStore(context->builder, value, pointer);
	LLVMSetAlignment(ref, alignment);
}

void llvm_store_aligned_decl(GenContext *context, Decl *decl, LLVMValueRef value)
{
	llvm_store_aligned(context, decl->backend_ref, value, decl->alignment);
}

void llvm_memcpy_to_decl(GenContext *context, Decl *decl, LLVMValueRef source, unsigned source_alignment)
{
	if (source_alignment == 0) source_alignment = type_abi_alignment(decl->type);
	LLVMBuildMemCpy(context->builder, decl->backend_ref, decl->alignment ?: type_abi_alignment(decl->type),
	                source, source_alignment, llvm_int(type_usize, type_size(decl->type)));
}

LLVMValueRef gencontext_emit_load_aligned(GenContext *context, LLVMTypeRef type, LLVMValueRef pointer, unsigned alignment, const char *name)
{
	LLVMValueRef value = LLVMBuildLoad2(context->builder, type, pointer, name);
	LLVMSetAlignment(value, alignment ?: llvm_abi_alignment(type));
	return value;
}

unsigned llvm_store_size(LLVMTypeRef type)
{
	return LLVMStoreSizeOfType(target_data_layout(), type);
}

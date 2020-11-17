// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"
#include "bigint.h"

bool gencontext_check_block_branch_emit(GenContext *context)
{
	if (!context->current_block) return false;
	// If it's not used, we can delete the previous block and skip the branch.
	// Unless it is the entry block or a label target for jumps
	// These empty blocks will occur when doing branches.
	// Consider:
	// while (1)
	// {
	//   break;
	//   break;
	// }
	// Naively we'd output
	// br label %for.cond  - 1st break
	// br label %for.cond  - 2nd break
	// br label %for.cond  - end of scope
	//
	// The fix is to introduce a new block after a break:
	// br label %for.cond
	// jmp:
	// br label %for.cond
	// jmp.1:
	// br label %for.cond
	//
	// But this leaves us with blocks that have no parent.
	// Consequently we will delete those and realize that
	// we then have no need for emitting a br.
	if (!context->current_block_is_target
	    && !LLVMGetFirstUse(LLVMBasicBlockAsValue(context->current_block)))
	{
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = NULL;
		return false;
	}
	return true;
};

void gencontext_emit_br(GenContext *context, LLVMBasicBlockRef next_block)
{
	if (!gencontext_check_block_branch_emit(context)) return;
	context->current_block = NULL;
	LLVMBuildBr(context->builder, next_block);
}



void gencontext_emit_cond_br(GenContext *context, LLVMValueRef value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
	assert(context->current_block);
	LLVMBuildCondBr(context->builder, value, then_block, else_block);
	LLVMClearInsertionPosition(context->builder);
	context->current_block = NULL;
	context->current_block_is_target = false;
}

void gencontext_emit_trunc_cond_br(GenContext *context, LLVMValueRef value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
	value = LLVMBuildTrunc(context->builder, value, context->bool_type, "");
	gencontext_emit_cond_br(context, value, then_block, else_block);
}


void gencontext_emit_block(GenContext *context, LLVMBasicBlockRef next_block)
{
	assert(context->current_block == NULL);
	LLVMAppendExistingBasicBlock(context->function, next_block);
	LLVMPositionBuilderAtEnd(context->builder, next_block);
	context->current_block = next_block;
	context->current_block_is_target = false;
}

static void gencontext_expand_from_args(GenContext *context, Type *type, LLVMValueRef ref, unsigned *index)
{
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
			for (unsigned i = 0; i < type->array.len; i++)
			{
				LLVMValueRef indices[2] = { gencontext_emit_const_int(context, type_uint, 0), gencontext_emit_const_int(context, type_uint, i) };
				LLVMValueRef target = LLVMBuildInBoundsGEP2(context->builder, llvm_type(type), ref, indices, 2, "");
				LLVMValueRef cast_addr = gencontext_emit_bitcast(context, target, type_get_ptr(type->array.base));
				gencontext_expand_from_args(context, type->array.base, cast_addr, index);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				LLVMValueRef indices[2] = { gencontext_emit_const_int(context, type_uint, 0), gencontext_emit_const_int(context, type_uint, i) };
				LLVMValueRef target = LLVMBuildInBoundsGEP2(context->builder, llvm_type(type), ref, indices, 2, "");
				LLVMValueRef cast_addr = gencontext_emit_bitcast(context, target, type_get_ptr(members[i]->type));
				gencontext_expand_from_args(context, members[i]->type, cast_addr, index);
			}
			return;
		}
		case TYPE_UNION:
			/*
			 * 			auto largestSize = DataSize::Zero();
			Type largestType = VoidTy;

			for (const auto field: type.unionMembers()) {
				// Skip zero length bitfields.
				if (field.isBitField() &&
				    field.bitFieldWidth().asBits() == 0) {
					continue;
				}
				assert(!field.isBitField() &&
				       "Cannot expand structure with bit-field members.");
				const auto fieldSize = typeInfo.getTypeAllocSize(field.type());
				if (largestSize < fieldSize) {
					largestSize = fieldSize;
					largestType = field.type();
				}
			}

			if (largestType == VoidTy) {
				return;
			}

			const auto irType = typeInfo.getLLVMType(largestType);
			const auto castAddress = builder.getBuilder().CreateBitCast(alloca,
			                                                            irType->getPointerTo());
			expandTypeFromArgs(typeInfo, builder, largestType,
			                   castAddress, iterator);
			 */
			TODO
		default:
			LLVMBuildStore(context->builder, LLVMGetParam(context->function, (*index)++), ref);
			return;

	}
}

static LLVMValueRef gencontext_get_next_parameter(GenContext *context, unsigned *index)
{
	return LLVMGetParam(context->function, (*index)++);
}


static inline void gencontext_process_parameter_value(GenContext *context, Decl *decl, unsigned *index)
{
	ABIArgInfo *info = decl->var.abi_info;
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			return;
		case ABI_ARG_DIRECT:
			llvm_store_aligned_decl(context, decl, gencontext_get_next_parameter(context, index));
			return;
		case ABI_ARG_INDIRECT:
		{
			// A simple memcopy, with alignment respected.
			LLVMValueRef pointer = gencontext_get_next_parameter(context, index);
			llvm_memcpy_to_decl(context, decl, pointer, info->indirect.realignment);
			return;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			// Here we do the following transform:
			// lo, hi -> { lo, hi } -> struct
			LLVMTypeRef lo = llvm_abi_type(info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(info->direct_pair.hi);
			LLVMTypeRef struct_type = gencontext_get_twostruct(context, lo, hi);
			unsigned decl_alignment = decl_abi_alignment(decl);
			// Cast to { lo, hi }
			LLVMValueRef cast = LLVMBuildBitCast(context->builder, decl->backend_ref, LLVMPointerType(struct_type, 0), "pair");
			// Point to the lo value.
			LLVMValueRef lo_ptr = LLVMBuildStructGEP2(context->builder, struct_type, cast, 0, "lo");
			// Store it in the struct.
			unsigned lo_alignment = MIN(llvm_abi_alignment(lo), decl_alignment);
			llvm_store_aligned(context, lo_ptr, gencontext_get_next_parameter(context, index), lo_alignment);
			// Point to the hi value.
			LLVMValueRef hi_ptr = LLVMBuildStructGEP2(context->builder, struct_type, cast, 1, "hi");
			// Store it in the struct.
			unsigned hi_alignment = MIN(llvm_abi_alignment(hi), decl_alignment);
			llvm_store_aligned(context, hi_ptr, gencontext_get_next_parameter(context, index), decl_alignment);
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = gencontext_get_coerce_type(context, info);
			// Cast to the coerce type.
			LLVMValueRef cast = LLVMBuildBitCast(context->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");
			unsigned decl_alignment = decl_abi_alignment(decl);

			// If we're not flattening, we simply do a store.
			if (!abi_info_should_flatten(info))
			{
				LLVMValueRef param = gencontext_get_next_parameter(context, index);
				// Store it with the alignment of the decl.
				llvm_store_aligned_decl(context, decl, param);
				return;
			}

			// In this case we've been flattening the parameter into multiple registers.
			LLVMTypeRef element_type = llvm_abi_type(info->direct_coerce.type);
			// Store each expanded parameter.
			for (unsigned idx = 0; idx < info->direct_coerce.elements; idx++)
			{
				LLVMValueRef element_ptr = LLVMBuildStructGEP2(context->builder, coerce_type, cast, idx, "");
				LLVMValueRef value = gencontext_get_next_parameter(context, index);

				llvm_store_aligned(context, element_ptr, value, MIN(llvm_abi_alignment(element_type), decl_alignment));
			}
			return;
		}
		case ABI_ARG_EXPAND:
		{
			gencontext_expand_from_args(context, decl->type, decl->backend_ref, index);
			if (info->expand.padding_type)
			{
				// Skip the pad.
				gencontext_get_next_parameter(context, index);
			}
		}
	}
}
static inline void gencontext_emit_parameter(GenContext *context, Decl *decl, unsigned *index)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

	const char *name = decl->name ? decl->name : "anon";
	// Allocate room on stack, but do not copy.
	decl->backend_ref = gencontext_emit_decl_alloca(context, decl);
	gencontext_process_parameter_value(context, decl, index);
	if (gencontext_use_debug(context))
	{
		SourceLocation *loc = TOKLOC(decl->span.loc);
		LLVMMetadataRef var = LLVMDIBuilderCreateParameterVariable(
				context->debug.builder,
				context->debug.function,
				name,
				strlen(name),
				*index + 1, // <- incorrect
				context->debug.file,
				loc->line,
				gencontext_get_debug_type(context, decl->type),
				true, 0 /* flags */
		                                                          );
		decl->var.backend_debug_ref = var;
		LLVMDIBuilderInsertDeclareAtEnd(context->debug.builder,
		                                decl->backend_ref, var, LLVMDIBuilderCreateExpression(context->debug.builder, NULL, 0),
		                                LLVMDIBuilderCreateDebugLocation(context->context, loc->line, loc->col, context->debug.function, /* inline at */NULL),
		                                LLVMGetInsertBlock(context->builder));
	}

}

void gencontext_emit_return_abi(GenContext *context, LLVMValueRef return_value, LLVMValueRef failable)
{
	FunctionSignature *signature = &context->cur_func_decl->func.function_signature;
	ABIArgInfo *info = signature->ret_abi_info;

	// If we have a failable it's always the return argument, so we need to copy
	// the return value into the return value holder.
	LLVMValueRef return_out = context->return_out;
	Type *return_type = signature->rtype->type;

	// In this case we use the failable as the actual return.
	if (signature->failable)
	{
		if (return_value)
		{
			LLVMBuildStore(context->builder, return_value, context->return_out);
		}
		return_out = context->failable_out;
		return_type = type_error;
		return_value = failable;
		info = signature->failable_abi_info;
	}

	switch (info->kind)
	{
		case ABI_ARG_INDIRECT:
			LLVMBuildStore(context->builder, return_value, return_out);
			if (info->indirect.realignment) TODO
			gencontext_emit_return_value(context, NULL);
			return;
		case ABI_ARG_IGNORE:
			gencontext_emit_return_value(context, NULL);
			return;
		case ABI_ARG_EXPAND:
			// Expands to multiple slots -
			// Not applicable to return values.
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_DIRECT_COERCE:
		{
			assert(!abi_info_should_flatten(info));
			LLVMTypeRef coerce_type = gencontext_get_coerce_type(context, info);
			gencontext_emit_return_value(context, gencontext_emit_convert_value_to_coerced(context, coerce_type, return_value, return_type));
			return;
		}
		case ABI_ARG_DIRECT:
			// The normal return
			gencontext_emit_return_value(context, return_value);
			return;
	}
	assert(return_value);
	Type *coerce_type = info->coerce_type;
	LLVMTypeRef coerce_llvm = llvm_type(coerce_type);
	LLVMTypeRef return_llvm_type = llvm_type(return_type);
	if (coerce_llvm == return_llvm_type && info->direct_offset == 0)
	{
		gencontext_emit_return_value(context, return_value);
	}
	// For more complex cases, store the value
	// into a temporary alloca and then perform
	// a coerced load from it.
	LLVMValueRef source = gencontext_emit_alloca(context, return_llvm_type, 0, "coerce");
	LLVMBuildStore(context->builder, return_value, source);
	/*
				const auto storeInst = createStore(builder_.getBuilder(), returnValue, sourcePtr);
				storeInst->setAlignment(typeInfo_.getTypeRequiredAlign(returnType).asBytes());

				auto sourceType = returnType;

				if (returnArgInfo.getDirectOffset() != 0) {
					sourcePtr = builder_.getBuilder().CreateBitCast(sourcePtr, llvm::PointerType::getUnqual(typeInfo_.getLLVMType(Int8Ty)));
					sourcePtr = builder_.getBuilder().CreateConstGEP1_32(sourcePtr, returnArgInfo.getDirectOffset());
					sourcePtr = builder_.getBuilder().CreateBitCast(sourcePtr, llvm::PointerType::getUnqual(typeInfo_.getLLVMType(coerceType)));
					sourceType = coerceType;
				}

				return createCoercedLoad(typeInfo_,
				                         builder_,
				                         sourcePtr,
				                         sourceType,
				                         coerceType);
			}

 */

	if (failable)
	{
		TODO
		// 		LLVMBuildRet(context->builder, );
	}

}
void gencontext_emit_implicit_return(GenContext *context)
{
	if (context->cur_func_decl->func.function_signature.rtype->type != type_void)
	{
		LLVMBuildUnreachable(context->builder);
		return;
	}
	LLVMValueRef failable = context->cur_func_decl->func.function_signature.failable ?
	                        gencontext_emit_no_error_union(context) : NULL;
	gencontext_emit_return_abi(context, NULL, failable);
}

void gencontext_emit_function_body(GenContext *context, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->external_name);
	assert(decl->backend_ref);

	bool emit_debug = gencontext_use_debug(context);
	LLVMValueRef prev_function = context->function;
	LLVMBuilderRef prev_builder = context->builder;

	context->error_var = NULL;
	context->catch_block = NULL;

	context->function = decl->backend_ref;
	if (emit_debug)
	{
		context->debug.function = LLVMGetSubprogram(context->function);
	}

	context->cur_func_decl = decl;

	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context->context, context->function, "entry");
	context->current_block = entry;
	context->current_block_is_target = true;
	context->expr_block_exit = NULL;
	context->builder = LLVMCreateBuilderInContext(context->context);
	LLVMPositionBuilderAtEnd(context->builder, entry);

	LLVMValueRef alloca_point = LLVMBuildAlloca(context->builder, LLVMInt32TypeInContext(context->context), "alloca_point");
	context->alloca_point = alloca_point;

	FunctionSignature *signature = &decl->func.function_signature;
	unsigned arg = 0;

	if (emit_debug)
	{
		gencontext_push_debug_scope(context, context->debug.function);
	}

	if (signature->failable && signature->failable_abi_info->kind == ABI_ARG_INDIRECT)
	{
		context->failable_out = LLVMGetParam(context->function, arg++);
	}
	else
	{
		context->failable_out = NULL;
	}
	if (signature->ret_abi_info && signature->ret_abi_info->kind == ABI_ARG_INDIRECT)
	{
		context->return_out = LLVMGetParam(context->function, arg++);
	}
	else
	{
		context->return_out = NULL;
	}


	// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
	VECEACH(decl->func.function_signature.params, i)
	{
		gencontext_emit_parameter(context, decl->func.function_signature.params[i], &arg);
	}

	LLVMSetCurrentDebugLocation2(context->builder, NULL);

	VECEACH(decl->func.body->compound_stmt.stmts, i)
	{
		gencontext_emit_stmt(context, decl->func.body->compound_stmt.stmts[i]);
	}

	if (context->current_block && !LLVMGetFirstInstruction(context->current_block) && !LLVMGetFirstUse(LLVMBasicBlockAsValue(context->current_block)))
	{
		LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(context->current_block);
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = prev_block;
		LLVMPositionBuilderAtEnd(context->builder, context->current_block);
	}
	// Insert a return (and defer) if needed.
	if (context->current_block && !LLVMGetBasicBlockTerminator(context->current_block))
	{
		assert(!decl->func.body->compound_stmt.defer_list.end);
		gencontext_emit_defer(context, decl->func.body->compound_stmt.defer_list.start, 0);
		gencontext_emit_implicit_return(context);
	}

	// erase alloca point
	if (LLVMGetInstructionParent(alloca_point))
	{
		context->alloca_point = NULL;
		LLVMInstructionEraseFromParent(alloca_point);
	}

	LLVMDisposeBuilder(context->builder);

	if (gencontext_use_debug(context))
	{
		gencontext_pop_debug_scope(context);
	}

	context->builder = prev_builder;
	context->function = prev_function;
}

void gencontext_emit_param_attributes(GenContext *context, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index)
{
	assert(last_index == index || info->kind == ABI_ARG_DIRECT_PAIR || info->kind == ABI_ARG_IGNORE
	       || info->kind == ABI_ARG_EXPAND);

	if (info->attributes.zeroext)
	{
		// Direct only
		assert(index == last_index);
		gencontext_add_attribute(context, function, zext_attribute, index);
	}
	if (info->attributes.signext)
	{
		// Direct only
		assert(index == last_index);
		gencontext_add_attribute(context, function, sext_attribute, index);
	}
	switch (info->kind)
	{
		case ABI_ARG_EXPAND:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_PAIR:
			break;
		case ABI_ARG_INDIRECT:
			if (info->indirect.realignment)
			{
				gencontext_add_int_attribute(context, function, align_attribute, info->indirect.realignment, index);
			}
			if (is_return)
			{
				gencontext_add_attribute(context, function, sret_attribute, index);
			}
			else
			{
				// TODO then type attributes are added to LLVM-C, use that for byval.
				if (info->indirect.by_val) gencontext_add_attribute(context, function, byval_attribute, index);
				gencontext_add_attribute(context, function, noalias_attribute, index);
			}
			break;

	}

}
void gencontext_emit_function_decl(GenContext *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.
	LLVMValueRef function = LLVMAddFunction(context->module, decl->cname ?: decl->external_name, llvm_type(decl->type));
	decl->backend_ref = function;
	FunctionSignature *signature = &decl->func.function_signature;
	ABIArgInfo *ret_abi_info = signature->failable_abi_info ?: signature->ret_abi_info;
	gencontext_emit_param_attributes(context, function, ret_abi_info, true, 1, 1);
	Decl **params = signature->params;
	if (signature->failable_abi_info && signature->ret_abi_info)
	{
		ABIArgInfo *info = signature->ret_abi_info;
		gencontext_emit_param_attributes(context, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	VECEACH(params, i)
	{
		Decl *param = params[i];
		ABIArgInfo *info = param->var.abi_info;
		gencontext_emit_param_attributes(context, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	if (decl->func.attr_inline)
	{
		gencontext_add_attribute(context, function, alwaysinline_attribute, -1);
	}
	if (decl->func.attr_noinline)
	{
		gencontext_add_attribute(context, function, noinline_attribute, -1);
	}
	if (decl->func.attr_noreturn)
	{
		gencontext_add_attribute(context, function, noreturn_attribute, -1);
	}
	if (decl->alignment)
	{
		LLVMSetAlignment(function, decl->alignment);
	}
	if (decl->section)
	{
		LLVMSetSection(function, decl->section);
	}
	gencontext_add_attribute(context, function, nounwind_attribute, -1);

	if (decl->func.attr_stdcall && (build_target.os == OS_TYPE_WIN32))
	{
		LLVMSetFunctionCallConv(function, LLVMX86StdcallCallConv);
		LLVMSetDLLStorageClass(function, LLVMDLLImportStorageClass);
	}

	switch (decl->visibility)
	{
		case VISIBLE_EXTERN:
			LLVMSetLinkage(function, decl->func.attr_weak ? LLVMExternalWeakLinkage : LLVMExternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_MODULE:
			if (decl->func.attr_weak) LLVMSetLinkage(function, LLVMWeakAnyLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetLinkage(function, decl->func.attr_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;;
	}
	if (context->debug.builder)
	{
		LLVMDIFlags flags = LLVMDIFlagZero;
		if (!decl->func.body) flags |= LLVMDIFlagPrototyped;
		switch (decl->visibility)
		{
			case VISIBLE_LOCAL:
			case VISIBLE_EXTERN:
				flags |= LLVMDIFlagPrivate;
				break;
			case VISIBLE_MODULE:
				flags |= LLVMDIFlagProtected;
				break;
			case VISIBLE_PUBLIC:
				flags |= LLVMDIFlagPublic;
				break;
		}
		flags |= LLVMDIFlagPrototyped;
		SourceLocation *loc = TOKILOC(decl->span.loc);
		context->debug.function = LLVMDIBuilderCreateFunction(context->debug.builder,
		                                                      context->debug.file,
		                                                      decl->name, TOKILEN(decl->name_token),
		                                                      decl->external_name, strlen(decl->external_name),
		                                                      context->debug.file,
		                                                      loc->line,
		                                                      llvm_debug_type(decl->type),
		                                                      decl->visibility == VISIBLE_LOCAL,
		                                                      true,
		                                                      loc->line,
		                                                      flags,
		                                                      build_options.optimization_level != OPTIMIZATION_NONE);
		LLVMSetSubprogram(function, context->debug.function);
	}
}



void gencontext_emit_extern_decl(GenContext *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			UNREACHABLE;
		case DECL_FUNC:
			decl->backend_ref = LLVMAddFunction(context->module, decl->cname ?: decl->external_name,
			                                    llvm_type(decl->type));
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case DECL_VAR:
			decl->backend_ref = LLVMAddGlobal(context->module, llvm_type(decl->type), decl->cname ?: decl->external_name);
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case DECL_TYPEDEF:
			UNREACHABLE
		case DECL_ENUM_CONSTANT:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERR:
			llvm_type(decl->type);
			TODO // Fix typeid
			break;
		case DECL_ENUM:
			TODO
		case NON_TYPE_DECLS:
			UNREACHABLE
	}
}

/*
typedef enum
{
	CABI_ATTRIBUTE_NONE,
	CABI_ATTRIBUTE_SRETURN,
	CABI_ATTRIBUTE_NEST,
	CABI_ATTRIBUTE_BY_VAL,
} CABIAttribute;

typedef enum
{
	LLVM_ARG_EMPTY,
	LLVM_ARG_SSE,
	LLVM_ARG_SIMD,
	LLVM_ARG_INT,
} LLVMArgCategory;

typedef struct
{
	LLVMTypeRef direct_type;
	CABIAttribute attribute;
	Type** types;
} EightByteChunk;

typedef struct
{

} FunctionArgumentRules;

typedef enum
{
	PASS_DIRECT,
	PASS_IGNORE,
	PASS_INDIRECT,
} FunctionArgPassing;


typedef struct
{
	union
	{
		LLVMTypeRef type;
		LLVMTypeRef *types;
	};
	FunctionArgPassing passing;
	CABIAttribute attribute;
	int offset;
} CABIParamInfo;

static LLVMArgCategory function_arg_common(EightByteChunk* chunk)
{
	LLVMArgCategory category = LLVM_ARG_EMPTY;
	VECEACH(chunk->types, i)
	{
		Type *t = chunk->types[i];
		category = display_meet(type_display(t), category);
	}
	return category;
}

static inline CABIParamInfo cabi_param(LLVMTypeRef type, FunctionArgPassing passing, CABIAttribute attribute, int offset)
{
	return (CABIParamInfo) { .type = type, .passing = passing, .attribute = attribute, .offset = offset };
}
static inline CABIParamInfo cabi_param_ignored()
{
	return cabi_param(NULL, PASS_IGNORE, CABI_ATTRIBUTE_NONE, -1);
}

static inline CABIParamInfo cabi_params(LLVMTypeRef *types, FunctionArgPassing passing, CABIAttribute attribute, int offset)
{
	return (CABIParamInfo) { .types = types, .passing = passing, .attribute = attribute, .offset = offset };
}

typedef struct
{
	int number;
	LLVMTypeRef type;
} HFAInfo;


				case llvm::CallingConv::X86_64_SysV:
					availIntRegs_ = 6;
					availSSERegs_ = 8;
					break;
				case llvm::CallingConv::ARM_AAPCS:
					availIntRegs_ = 8;
					availSIMDFPRegs_ = 8;
					break;
				default:
					llvm::errs() << "unsupported llvm::CallingConv::ID " << cconv << "\n";
					break;
			}
		}
		void addDirectIntArg() {
			if (availIntRegs_)
				availIntRegs_ -= 1;
			argCount_ += 1;
		}
		void addDirectSSEArg() {
			if (availSSERegs_)
				availSSERegs_ -= 1;
			argCount_ += 1;
		}
		// For ARM_AAPCS HFA, one argument may takes multiple registers.
		void addDirectSIMDFPArg(unsigned sr = 1) {
			unsigned t = availSIMDFPRegs_ - sr;
			if (availSIMDFPRegs_ > t)
				availSIMDFPRegs_ = t;
			argCount_ += 1;
		}
		void addIndirectArg() { argCount_ += 1; }
		void addIndirectReturn() {
			if (availIntRegs_)
				availIntRegs_ -= 1;
			argCount_ += 1;
		}
		// ARM_AAPCS uses separate x8 to store return address.
		void addIndirectReturnForARM_AAPCS() { argCount_ += 1; }
		void addChainArg() { argCount_ += 1; }
		unsigned argCount() const { return argCount_; }
		unsigned availIntRegs() const { return availIntRegs_; }
		unsigned availSSERegs() const { return availSSERegs_; }
		unsigned availSIMDFPRegs() const { return availSIMDFPRegs_; }
		void clearAvailIntRegs() { availIntRegs_ = 0; }
		void clearAvailSIMDFPRegs() { availSIMDFPRegs_ = 0; }
		private:
		unsigned availIntRegs_;
		unsigned availSSERegs_;
		unsigned availSIMDFPRegs_;
		unsigned argCount_;
};*//*


static EightByteChunk** func_arg_analyze_argument(GenContext *context, Type *type, HFAInfo *info)
{
	TODO
	return NULL;
}

static inline FunctionArgPassing func_arg_analyse_passing_x64(Type *type)
{
	unsigned size = type == type_void ? 0 : type_size(type);
	if (size == 0) return PASS_IGNORE;
	return size > 16 ? PASS_INDIRECT : PASS_DIRECT;
}

static inline CABIParamInfo func_arg_analyze_return_x64(GenContext *context, Type *type)
{
	switch (func_arg_analyse_passing_x64(type))
	{
		case PASS_IGNORE:
			return cabi_param_ignored();
		case PASS_INDIRECT:
			// Pass by ref
			if (context->abi.int_registers) context->abi.int_registers--;
			context->abi.args++;
			return cabi_param(llvm_type(type_get_ptr(type)), PASS_INDIRECT, CABI_ATTRIBUTE_SRETURN, 0);
		case PASS_DIRECT:
		{
			HFAInfo info;
			EightByteChunk** chunks = func_arg_analyze_argument(context, type, &info);
			switch (vec_size(chunks))
			{
				case 1:
					return cabi_param(chunks[0]->direct_type, PASS_DIRECT, chunks[0]->attribute, -1);
				case 2:
				{
					LLVMTypeRef types[2] = { chunks[0]->direct_type, chunks[1]->direct_type };
					LLVMTypeRef abi_type = LLVMStructTypeInContext(context->context, types, 2, false);
					return cabi_param(abi_type, PASS_DIRECT, CABI_ATTRIBUTE_NONE, -1);
				}
				default:
					UNREACHABLE
			}
		}
	}
}

static void func_arg_add_indirect_return_arm_aapcs(FunctionArgumentRules *rules)
{
	TODO
}

static void func_arg_get_register_requirements_x64(EightByteChunk **chunks, unsigned *int_regs, unsigned *sse_regs)
{
	*int_regs = 0;
	*sse_regs = 0;
	VECEACH(chunks, i)
	{
		EightByteChunk *chunk = chunks[i];
		if (chunks->getRegionTypDisp() == LLVM_ARG_SSE)
		{
			*sse_regs += 1;
		}
		else
		{
			*int_regs += 1;
		}
	}
}

static inline bool func_arg_can_pass_directly_x64(GenContext *context, unsigned int_regs, unsigned sse_regs)
{
	if (int_regs + sse_regs == 1) return true;
	return int_regs <= context->abi.int_registers && sse_regs <= context->abi.sse_registers;
}

static CABIParamInfo func_arg_analyze_abi_param_x64(GenContext *context, Type *type)
{
	*/
/*
	 * 	assert(paramType->flavor() != Btype::AuxT || ptyp->isVoidTy() ||
		       !(ptyp->isStructTy() || ptyp->isArrayTy() || ptyp->isVectorTy() ||
		         ptyp->isEmptyTy() || ptyp->isIntegerTy(8) || ptyp->isIntegerTy(16)));

	 *//*

	int sig_offset = context->abi.args;
	switch (func_arg_analyse_passing_x64(type))
	{
		case PASS_IGNORE:
			return cabi_param_ignored();
		case PASS_INDIRECT:
			// Pass in memory on the stack
			if (context->abi.int_registers) context->abi.int_registers--;
			context->abi.args++;
			return cabi_param(llvm_type(type_get_ptr(type)), PASS_INDIRECT, CABI_ATTRIBUTE_BY_VAL, sig_offset);
		case PASS_DIRECT:
			break;
		default:
			UNREACHABLE
	}
	HFAInfo info;
	EightByteChunk** chunks = func_arg_analyze_argument(context, type, &info);
	// Figure out how many registers it would take to pass this parm directly
	unsigned int_regs = 0;
	unsigned sse_regs = 0;
	func_arg_get_register_requirements_x64(chunks, &int_regs, &sse_regs);
	// Make direct/indirect decision
	CABIAttribute attr = CABI_ATTRIBUTE_NONE;
	if (func_arg_can_pass_directly_x64(context, int_regs, sse_regs))
	{
		LLVMTypeRef *types = NULL;
		VECEACH(chunks, i)
		{
			EightByteChunk *chunk = chunks[i];
			vec_add(types, chunk->direct_type);
			if (chunk->attribute != ATTRIBUTE_NONE)
			{
				attr == chunk->attribute;
			}
			if (chunk->region_type == LLVM_ARG_SSE)
			{
				if (context->abi.sse_registers) context->abi.sse_registers--;
				context->abi.args++;
			}
			else
			{
				if (context->abi.int_registers) context->abi.int_registers--;
				context->abi.int_registers++;
			}
		}
		return cabi_params(types, PASS_DIRECT, attr, sig_offset);
	}

	context->abi.args++;
	return cabi_param(llvm_type(type_get_ptr(type)), PASS_INDIRECT, CABI_ATTRIBUTE_BY_VAL, sig_offset);
}

static void func_arg_analyze_abi_param(GenContext *context, Type *type)
{
	if (build_target.arch == ARCH_TYPE_X86_64 && build_target.os != OS_TYPE_WIN32)
	{
		func_arg_analyze_abi_param_x64(context, type);
	}
	TODO
}

static inline CABIParamInfo func_arg_analyze_return_arm_aapcs(GenContext *context, FunctionArgumentRules *rules, Type *type)
{
	unsigned size = type == type_void ? 0 : type_size(type);
	if (size == 0) return cabi_param(llvm_type(type_void), PASS_IGNORE, CABI_ATTRIBUTE_NONE, -1);
	// Right now we don't have any float > 64 bits:
	if (size > 32)
	{
		// Add indirect return
		context->abi.args++;
		return cabi_param(llvm_type(type_get_ptr(type)), PASS_INDIRECT, CABI_ATTRIBUTE_SRETURN, 0);
	}
	HFAInfo info;
	EightByteChunk** chunks = func_arg_analyze_argument(context, type, &info);
	switch (info.number)
	{
		case 0:
			break;
		case 1:
			return cabi_param(info.type, PASS_DIRECT, CABI_ATTRIBUTE_NONE, -1);
		default:
		{
			LLVMTypeRef *types = malloc_arena(sizeof(LLVMTypeRef) * info.number);
			for (int i = 0; i < info.number; i++) types[i] = info.type;
			LLVMTypeRef abi_type = LLVMStructTypeInContext(context->context, types, info.number, false);
			return cabi_param(abi_type, PASS_DIRECT, CABI_ATTRIBUTE_NONE, -1);
		}
	}
	// The return value is not an HFA and its size exceeds 16 bytes,
	// be passed in memory, via a hidden struct return param.
	if (size > 16)
	{
		context->abi.args++;
		return cabi_param(llvm_type(type_get_ptr(type)), PASS_INDIRECT, CABI_ATTRIBUTE_SRETURN, 0);
	}
	switch (vec_size(chunks))
	{
		case 1:
			return cabi_param(chunks[0]->direct_type, PASS_DIRECT, chunks[0]->attribute, -1);
		case 2:
		{
			LLVMTypeRef types[2] = { chunks[0]->direct_type, chunks[1]->direct_type };
			LLVMTypeRef abi_type = LLVMStructTypeInContext(context->context, types, 2, false);
			return cabi_param(abi_type, PASS_DIRECT, CABI_ATTRIBUTE_NONE, -1);
		}
		default:
			UNREACHABLE
	}
}

*/
/**
 * CABIParamInfo CABIOracleARM_AAPCS::analyzeABIReturn(Btype *resultType,


  // Direct case
  auto &regions = ebi.regions();
  if (regions.size() == 1) {
    // Single value
    return CABIParamInfo(regions[0].abiDirectType, ParmDirect, regions[0].attr,
                         -1);
  }
  // Two-element struct
  assert(regions.size() == 2);
  llvm::Type *abiTyp = tm_->makeLLVMTwoElementStructType(
      regions[0].abiDirectType, regions[1].abiDirectType);
  return CABIParamInfo(abiTyp, ParmDirect, AttrNone, -1);
}
 * @param context
 * @param signature
 * @param rules
 *//*

void func_arg_create_c_abi(GenContext *context, FunctionSignature *signature, FunctionArgumentRules *rules)
{
	CABIParamInfo return_param;

	context->abi.args = 0;
	CABIParamInfo* params = NULL;
	switch (build_target.arch)
	{
		case ARCH_TYPE_X86_64:
			if (build_target.os == OS_TYPE_WIN32) TODO
			context->abi.int_registers = 6;
			context->abi.simd_registers = 0;
			context->abi.sse_registers = 8;
			vec_add(params, func_arg_analyze_return_x64(context, signature->rtype->type));
			break;
		default:
			context->abi.int_registers = 8;
			context->abi.simd_registers = 8;
			context->abi.sse_registers = 0;
			vec_add(params, func_arg_analyze_return_arm_aapcs(context, rules, signature->rtype->type));
			break;
	}

	int sig_offset = context->abi.args;
	context->abi.args++;
	vec_add(params, cabi_param(llvm_type(type_voidptr), PASS_DIRECT, CABI_ATTRIBUTE_NEST, sig_offset));

	// Now process the params.
	unsigned num_params = vec_size(signature->params);
	for (unsigned i = 0; i < num_params; i++)
	{
		Type *type = signature->params[i]->type;
		vec_add(params, func_arg_analyze_abi_param(context, type));
	}
	LLVMTypeRef *types = NULL;
	LLVMTypeRef return_type = NULL;
	if (params[0].passing == PASS_INDIRECT)
	{
		return_type = llvm_type(type_void);
		vec_add(types, params[0].type);
	}
	else
	{
		return_type = params[0].type;
	}
	unsigned expanded_parameters = vec_size(params);
	for (unsigned i = 1; i < expanded_parameters; i++)
	{
		if (params[i].passing == PASS_IGNORE) continue;
		TODO
		*/
/*
		 *     for (auto &abit : infov_[pidx].abiTypes())
      elems.push_back(abit);
		*//*

	}

}*/

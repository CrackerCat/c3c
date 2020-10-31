// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"

#define MIN_ABI_STACK_ALIGN 4

static unsigned x86_stack_alignment(Type *type, unsigned alignment)
{
	// Less than ABI, use default
	if (alignment < MIN_ABI_STACK_ALIGN) return 0;

	// On non-Darwin, the stack type alignment is always 4.
	if (!build_target.x86.is_darwin_vector_abi) return MIN_ABI_STACK_ALIGN;

	// Otherwise, if the type contains an SSE vector type, the alignment is 16.
	if (alignment >= 16 && (type_is_vector(type) || type_is_structlike_with_vector(type)))
	{
		return 16;
	}
	return MIN_ABI_STACK_ALIGN;
}


static ABIArgInfo *x86_create_indirect_return_result(GenContext *context, Type *type, bool by_val)
{
	if (!by_val)
	{
		if (context->abi.int_registers)
		{
			context->abi.int_registers--;
			if (!build_target.x86.is_mcu_api)
			{
				return create_abi_arg_indirect(type_abi_alignment(type), false, false, NULL);
			}
		}
		return create_abi_arg_indirect(type_abi_alignment(type), true, false, NULL);
	}

	// Compute alignment
	unsigned alignment = type_abi_alignment(type);
	unsigned stack_alignment = x86_stack_alignment(type, alignment);

	// Default alignment
	if (stack_alignment == 0)
	{
		return create_abi_arg_indirect(4, true, false, NULL);
	}
	// Realign if alignment is greater.
	bool realign = alignment > stack_alignment;
	return create_abi_arg_indirect(stack_alignment, true, realign, NULL);
}


ABIArgInfo *create_indirect_return_x86(GenContext *context, Type *type)
{
	bool by_val = true;
	if (context->abi.int_registers)
	{
		// Consume a register for the return.
		context->abi.int_registers--;
		by_val = build_target.x86.is_mcu_api;
	}
	return create_abi_arg_indirect(type_abi_alignment(type), by_val, false, NULL);
}

static bool x86_is_return_type_in_reg(Type *type)
{
	type = type->canonical;
	unsigned size = type_size(type);
	if (build_target.x86.is_mcu_api)
	{
		if (size > 8) return false;
	}
	else
	{
		// Check if register sized.
		if (size != 1 && size != 2 && size != 4 && size != 8) return false;
	}
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_MEMBER:
		case TYPE_VOID:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case TYPE_TYPEINFO:
			UNREACHABLE
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
		case TYPE_ENUM:
		case TYPE_POINTER:
		case TYPE_TYPEID:
		case TYPE_VARARRAY:
			return true;
		case TYPE_ARRAY:
			return x86_is_return_type_in_reg(type->array.base);
		case TYPE_ERR_UNION:
		case TYPE_STRING:
		case TYPE_SUBARRAY:
			return false;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
			// Handle below
			break;
			// TODO if we have a vector type then false
	}
	Decl** members = type->decl->strukt.members;
	VECEACH (members, i)
	{
		if (!x86_is_return_type_in_reg(members[i]->type)) return false;
	}
	return true;
}

ABIArgInfo *x86_classify_return(GenContext *context, Type *type)
{
	if (type == type_void) return create_abi_arg_ignore();

	Type *base = NULL;
	unsigned elements = 0;
	if (context->abi.call_convention == CALL_CONVENTION_VECTOR || context->abi.call_convention == CALL_CONVENTION_REGCALL)
	{
		if (type_is_homogenous_aggregate(type, &base, &elements)) return create_abi_arg_direct(NULL, 0, NULL, true);
	}

	if (type_is_vector(type))
	{
		if (build_target.x86.is_darwin_vector_abi)
		{
			unsigned size = type_size(type);
			if (size == 16)
			{
				/** ABIArgInfo::getDirect(llvm::VectorType::get(
                  llvm::Type::getInt64Ty(getVMContext()), 2)); */
				TODO
			}
			// Always return in register if it fits in a general purpose
			// register, or if it is 64 bits and has a single element.
			/*if ((size == 8 || size == 16 || size == 32) ||
			    (size == 64 && ...  == 1))
				return ABIArgInfo::getDirect(llvm::IntegerType::get(getVMContext(),
				                                                    Size));*/
			TODO
			return create_indirect_return_x86(context, type);
		}
		return create_abi_arg_direct(NULL, 0, NULL, true);
	}
	if (type_is_aggregate(type))
	{
		// If we don't allow small structs in reg:
		if (!build_target.x86.return_small_struct_in_reg_abi) return create_indirect_return_x86(context, type);
		bool struct_like = type_is_structlike(type);

		// Ignore empty structlikes.
		if (struct_like && !vec_size(type->decl->strukt.members)) return create_abi_arg_ignore();

		// Check if we can return it in a register.
		if (x86_is_return_type_in_reg(type))
		{
			size_t size = type_size(type);
			// Special case is floats and pointers in single element structs.
			Type *single_element = type_find_single_element(type);
			if (single_element)
			{
				if ((type_is_float(single_element) && !build_target.x86.is_win32_float_struct_abi)
				    || (type_is_pointer(type)))
				{
					return create_abi_arg_direct(single_element, 0, NULL, true);
				}
			}
			// TODO this should generate stuff like i48?
			return create_abi_arg_direct(type_unsigned_int_by_bitsize(size * 8), 0, NULL, true);
		}
		return create_indirect_return_x86(context, type);
	}
	type = type_lowering(type);
	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_BOOL:
			return create_abi_arg_extend(type);
		default:
			return create_abi_arg_direct(NULL, 0, NULL, true);
	}

}

static inline bool x86_should_aggregate_use_direct(GenContext *context, Type *type, bool *in_reg, bool *needs_padding)
{
	// TODO
	return false;
}

static inline bool x86_is_mmxtype(Type *type)
{
	TODO
}

static inline bool x86_can_expand_indirect_arg(Type *type)
{
	// Clang: Test whether an argument type which is to be passed indirectly (on the
	// stack) would have the equivalent layout if it was expanded into separate
	// arguments. If so, we prefer to do the latter to avoid inhibiting
	// optimizations.
	if (!type_is_structlike(type)) return false;
	size_t size = 0;

	if (type->type_kind == TYPE_ERRTYPE) { TODO }

	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *member_type = type_lowering(members[i]->type);
		member_type = type_lowering(member_type);
		switch (member_type->type_kind)
		{
			case TYPE_I32:
			case TYPE_U32:
			case TYPE_F32:
				size += 4;
				break;
			case TYPE_U64:
			case TYPE_I64:
			case TYPE_F64:
				// Note, complex types would be fine if their element type are fine.
				// in Clang.
				size += 8;
				break;
			default:
				return false;
		}
	}
	return size == type_size(type);
}




static bool x86_update_free_regs(GenContext *context, Type *type)
{
	if (!build_target.x86.use_soft_float)
	{
		if (type_is_float(type)) return false;
	}

	unsigned size = type_size(type);
	if (!size) return false;

	unsigned size_in_regs = (size + 3) / 4;

	if (!build_target.x86.is_mcu_api)
	{
		if (size_in_regs > context->abi.int_registers)
		{
			context->abi.int_registers = 0;
			return false;
		}
	}
	else
	{
		// Clang: The MCU psABI allows passing parameters in-reg even if there are
		// earlier parameters that are passed on the stack. Also,
		// it does not allow passing >8-byte structs in-register,
		// even if there are 3 free registers available.
		if (size_in_regs > context->abi.int_registers || size_in_regs > 2) return false;
	}
	context->abi.int_registers -= size_in_regs;
	return true;
}

static ABIArgInfo *x86_classify_argument(GenContext *context, Type *type)
{
	// FIXME: Set alignment on indirect arguments.

	type = type_lowering(type);
	Type *base = NULL;
	unsigned elements = 0;
	if (context->abi.call_convention == CALL_CONVENTION_REGCALL && type_is_homogenous_aggregate(type, &base, &elements))
	{
		if (context->abi.sse_registers >= elements)
		{
			context->abi.sse_registers -= elements;
			if (type_is_builtin(type->type_kind) || type_is_vector(type)) return create_abi_arg_direct(NULL, 0, NULL, true);
			return create_abi_arg_expand();
		}
		return x86_create_indirect_return_result(context, type, false);
	}


	if (type_is_aggregate(type))
	{
		if (!build_target.x86.is_win_api && type_is_empty(type))
		{
			return create_abi_arg_ignore();
		}

		bool needs_padding = false;
		bool in_reg;

		if (x86_should_aggregate_use_direct(context, type, &in_reg, &needs_padding))
		{
			unsigned size_in_regs = (type_size(type) + 3) / 4;
			TODO
			/*
					SmallVector<llvm::Type*, 3> Elements(SizeInRegs, Int32);
			llvm::Type *Result = llvm::StructType::get(LLVMContext, Elements);
			if (InReg)
				return ABIArgInfo::getDirectInReg(Result);
			else
				return ABIArgInfo::getDirect(Result);*/

		}

		Type *padding_type = needs_padding ? type_int : NULL;

		// Clang: Expand small (<= 128-bit) record types when we know that the stack layout
		// of those arguments will match the struct. This is important because the
		// LLVM backend isn't smart enough to remove byval, which inhibits many
		// optimizations.
		// Don't do this for the MCU if there are still free integer registers
		// (see X86_64 ABI for full explanation).
		if (type_size(type) < 4 * 4 && (!build_target.x86.is_mcu_api || context->abi.sse_registers) &&
				x86_can_expand_indirect_arg(type))
		{
			bool pad;
			switch (context->abi.call_convention)
			{
				case CALL_CONVENTION_REGCALL:
				case CALL_CONVENTION_FAST:
				case CALL_CONVENTION_VECTOR:
					pad = true;
					break;
				default:
					pad = false;
					break;
			}
			return create_abi_expand(pad, padding_type);
		}
		return x86_create_indirect_return_result(context, type, true);
	}

	// Clang: On Darwin, some vectors are passed in memory, we handle this by passing
	// it as an i8/i16/i32/i64.
	if (type_is_vector(type))
	{
		if (build_target.x86.is_darwin_vector_abi)
		{
			unsigned size = type_size(type);
			if ((size == 1 || size == 2 || size == 4) || (size == 8 && /* elements == 1 */ false))
			{
				//return ABIArgInfo::getDirect(llvm::IntegerType::get(getVMContext(), Size));
				TODO
			}
		}
		if (x86_is_mmxtype(type))
		{
			// return ABIArgInfo::getDirect(llvm::IntegerType::get(getVMContext(), 64));
			TODO
		}

		return create_abi_arg_direct(NULL, 0, NULL, true);
	}

	bool in_reg = false;
	if (x86_update_free_regs(context, type)
	    && !build_target.x86.is_mcu_api)
	{
		switch (context->abi.call_convention)
		{
			case CALL_CONVENTION_FAST:
			case CALL_CONVENTION_VECTOR:
			case CALL_CONVENTION_REGCALL:
				if (type_size(type) > 4) break;
				in_reg = type_is_integer(type) || type_is_pointer(type);
				break;
			default:
				in_reg = true;
				break;
		}
	}

	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_BOOL:
			return in_reg ? create_abi_arg_extend_in_reg(type) : create_abi_arg_extend(type);
		default:
			return in_reg ? create_abi_arg_direct_in_reg() : create_abi_arg_direct(NULL, 0, NULL, true);
	}
}


static void x86_compute_vector_args(GenContext *context, ABIArgInfo ***args_infos, bool *used_in_alloca)
{
	TODO
}

void c_abi_func_create_x86(GenContext *context, FunctionSignature *signature)
{
	context->abi.call_convention = signature->convention;
	context->abi.sse_registers = 0;
	switch (signature->convention)
	{
		case CALL_CONVENTION_NORMAL:
		case CALL_CONVENTION_SYSCALL:
			context->abi.int_registers = build_target.default_number_regs;
			break;
		case CALL_CONVENTION_REGCALL:
			context->abi.int_registers = 5;
			context->abi.sse_registers = 8;
			break;
		case CALL_CONVENTION_VECTOR:
			context->abi.int_registers = 2;
			context->abi.sse_registers = 6;
			break;
		case CALL_CONVENTION_FAST:
			context->abi.int_registers = 2;
			break;
		default:
			UNREACHABLE
	}
	if (build_target.x86.is_mcu_api)
	{
		context->abi.sse_registers = 0;
		context->abi.int_registers = 3;
	}

	ABIArgInfo** args_infos = NULL;
	vec_add(args_infos, x86_classify_return(context, signature->rtype->type));

	bool used_in_alloca = false;

	if (context->abi.call_convention == CALL_CONVENTION_VECTOR)
	{
		x86_compute_vector_args(context, &args_infos, &used_in_alloca);
	}
	else
	{
		Decl **params = signature->params;
		VECEACH(params, i)
		{
			ABIArgInfo *info = x86_classify_argument(context, params[i]->type);
			params[i]->abi_ref = info;
			used_in_alloca |= (info->kind == ABI_ARG_ALLOCA);
		}
	}

	if (used_in_alloca)
	{
		TODO
		// rewriteWithInAlloca(FI);
	}

	Type *return_type = signature->rtype->type->canonical;
	int params = 0;
	/*
	if (signature->failable)
	{
		call_descriptor->failure = (CABIParam) { params++, CABI_PARAM_SRETURN };
	}
	// TODO support vectors when they're in the language
	if (type_is_structlike(return_type) || type_size(return_type) > 64)
	{
		call_descriptor->return_value = (CABIParam) { params, !params ? CABI_PARAM_SRETURN : CABI_PARAM_SRETURN};
	}
	TODO*/
}

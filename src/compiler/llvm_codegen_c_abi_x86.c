// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"

#define MIN_ABI_STACK_ALIGN 4

static bool x86_update_free_regs(GenContext *context, Type *type);

static inline bool is_register_sized(unsigned size)
{
	return size == 1 || size == 2 || size == 4 || size == 8;
}

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


static ABIArgInfo *x86_create_indirect_result(GenContext *context, Type *type, ByReg by_reg)
{
	if (by_reg == BY_REG)
	{
		ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
		if (!context->abi.int_registers) return info;

		context->abi.int_registers--;
		if (build_target.x86.is_mcu_api) return info;

		return abi_arg_by_reg_attr(info);
	}

	// Compute alignment
	unsigned alignment = type_abi_alignment(type);
	unsigned stack_alignment = x86_stack_alignment(type, alignment);

	// Default alignment
	if (stack_alignment == 0) stack_alignment = 4;

	// Realign if alignment is greater.
	if (alignment > stack_alignment)
	{
		return abi_arg_new_indirect_realigned(stack_alignment);
	}

	return abi_arg_new(ABI_ARG_INDIRECT);
}


ABIArgInfo *create_indirect_return_x86(GenContext *context)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	if (!context->abi.int_registers) return info;
	// Consume a register for the return.
	context->abi.int_registers--;
	if (build_target.x86.is_mcu_api) return info;

	return abi_arg_by_reg_attr(info);
}

static bool x86_should_return_type_in_reg(Type *type)
{
	type = type->canonical;
	unsigned size = type_size(type);
	if (build_target.x86.is_mcu_api && size > 8) return false;
	if (!build_target.x86.is_mcu_api && !is_register_sized(size)) return false;

	if (type->type_kind == TYPE_VECTOR)
	{
		// 64 (and 128 bit) vectors are not returned as registers
		return size != 8;
	}

	switch (type->type_kind)
	{
		case TYPE_VECTOR:
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
		case TYPE_ERR_UNION:
		case TYPE_STRING:
		case TYPE_SUBARRAY:
		case TYPE_ERRTYPE:
		case TYPE_COMPLEX:
			return true;
		case TYPE_ARRAY:
			return x86_should_return_type_in_reg(type->array.base);
		case TYPE_STRUCT:
		case TYPE_UNION:
			// Handle below
			break;
	}
	// If all can be passed in registers, then pass in register
	// (remember we already limited the size!)
	Decl** members = type->decl->strukt.members;
	VECEACH (members, i)
	{
		Type *member_type = members[i]->type;
		if (type_is_empty_field(member_type, true)) continue;
		if (!x86_should_return_type_in_reg(member_type)) return false;
	}
	return true;
}

ABIArgInfo *x86_classify_return(GenContext *context, Type *type)
{
	if (type == type_void)
	{
		return abi_arg_new(ABI_ARG_IGNORE);
	}

	Type *base = NULL;
	unsigned elements = 0;
	if (context->abi.call_convention == CALL_CONVENTION_VECTOR || context->abi.call_convention == CALL_CONVENTION_REGCALL)
	{
		// Pass in the normal way.
		if (type_is_homogenous_aggregate(type, &base, &elements))
		{
			return abi_arg_new(ABI_ARG_DIRECT);
		}
	}

	if (type_is_vector(type))
	{
		// On Darwin, vectors may be returned in registers.
		if (build_target.x86.is_darwin_vector_abi)
		{
			unsigned size = type_size(type);
			if (size == 128 / 8)
			{
				// Special case, convert 128 bit vector to two 64 bit elements.
				return abi_arg_new_direct_coerce(abi_type_new_plain(type_get_vector(type_long, 2)));
			}
			// Always return in register if it fits in a general purpose
			// register, or if it is 64 bits and has a single element.
			if (size == 1 || size == 2 || size == 4 || (size == 8 && type->vector.len == 1))
			{
				return abi_arg_new_direct_coerce(abi_type_new_int_bits(size * 8));
			}
			return create_indirect_return_x86(context);
		}
		return abi_arg_new(ABI_ARG_DIRECT);
	}

	if (type_is_abi_aggregate(type))
	{
		// If we don't allow small structs in reg:
		if (!build_target.x86.return_small_struct_in_reg_abi && !type_is_complex(type))
		{
			return create_indirect_return_x86(context);
		}
		// Ignore empty struct/unions
		if (type_is_empty_union_struct(type, true))
		{
			return abi_arg_new(ABI_ARG_IGNORE);
		}

		// Check if we can return it in a register.
		if (x86_should_return_type_in_reg(type))
		{
			size_t size = type_size(type);
			// Special case is floats and pointers in single element structs (except for MSVC)
			Type *single_element = type_find_single_struct_element(type);
			if (single_element)
			{
				if ((type_is_float(single_element) && !build_target.x86.is_win32_float_struct_abi))
				{
					return abi_arg_new(ABI_ARG_EXPAND);
				}
				if (type_is_pointer(type))
				{
					return abi_arg_new(ABI_ARG_EXPAND);
				}
			}
			// This is not a single element struct, so we wrap it in an int.
			return abi_arg_new_direct_coerce(abi_type_new_int_bits(size * 8));
		}
		return create_indirect_return_x86(context);
	}

	// We lower the enums
	type = type_lowering(type);

	// Is this small enough to need to be extended?
	if (type_is_promotable_integer(type))
	{
		return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
	}

	// If we support something like int128, then this is an indirect return.
	if (type_is_integer(type) && type->builtin.bitsize > 64) return create_indirect_return_x86(context);

	// Otherwise we expect to just pass this nicely in the return.
	return abi_arg_new(ABI_ARG_DIRECT);

}

static inline bool x86_should_aggregate_use_direct(GenContext *context, Type *type, bool *needs_padding)
{
	// On Windows, aggregates other than HFAs are never passed in registers, and
	// they do not consume register slots. Homogenous floating-point aggregates
	// (HFAs) have already been dealt with at this point.
	if (build_target.x86.is_win32_float_struct_abi && type_is_abi_aggregate(type)) return false;

	*needs_padding = false;

	if (!x86_update_free_regs(context, type)) return false;

	if (build_target.x86.is_mcu_api) return true;

	switch (context->abi.call_convention)
	{
		case CALL_CONVENTION_FAST:
		case CALL_CONVENTION_VECTOR:
		case CALL_CONVENTION_REGCALL:
			if (type_size(type) <= 4 && context->abi.int_registers)
			{
				*needs_padding = true;
			}
			return false;
		default:
			return true;
	}
}

static inline bool x86_is_mmxtype(Type *type)
{
	// Return true if the type is an MMX type <2 x i32>, <4 x i16>, or <8 x i8>.
	if (type->type_kind != TYPE_VECTOR) return false;
	if (type_size(type->vector.base) >= 8) return false;
	if (!type_is_integer(type->vector.base)) return false;
	return type_size(type) == 8;
}

static inline bool x86_can_expand_indirect_arg(Type *type)
{
	// Clang: Test whether an argument type which is to be passed indirectly (on the
	// stack) would have the equivalent layout if it was expanded into separate
	// arguments. If so, we prefer to do the latter to avoid inhibiting
	// optimizations.

	// Error unions can always be expanded.
	if (type->canonical->type_kind == TYPE_ERR_UNION) return true;

	if (!type_is_union_struct(type)) return false;

	size_t size = 0;
	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *member_type = type_lowering(members[i]->type);
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
				size += 8;
				break;
			case TYPE_COMPLEX:
				// Note, complex types would be fine if their element type are fine.
				// in Clang.
				TODO
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

static ABIArgInfo *x86_create_direct_hva(Type *base_type, unsigned elements)
{
	assert(base_type);
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = ABI_ARG_DIRECT_HVA;
	return info;
}

static ABIArgInfo *x86_classify_argument(GenContext *context, Type *type)
{
	// FIXME: Set alignment on indirect arguments.

	type = type_lowering(type);
	Type *base = NULL;
	unsigned elements = 0;
	bool is_reg_call = context->abi.call_convention == CALL_CONVENTION_REGCALL;
	bool is_vec_call = context->abi.call_convention == CALL_CONVENTION_VECTOR;
	bool is_fast_call = context->abi.call_convention == CALL_CONVENTION_FAST;
	if ((is_vec_call || is_reg_call)
		&& type_is_homogenous_aggregate(type, &base, &elements))
	{
		// We now know it's a float/double or a vector.
		if (context->abi.sse_registers >= elements)
		{
			context->abi.sse_registers -= elements;
			if (is_vec_call) return x86_create_direct_hva(base, elements);

			// If it doesn't need to expand
			if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_VECTOR)
			{
				return abi_arg_new(ABI_ARG_DIRECT);
			}

			// Otherwise just a normal expand.
			return abi_arg_new(ABI_ARG_EXPAND);
		}
		return x86_create_indirect_result(context, type, BY_REG);
	}

	unsigned size = type_size(type);

	if (type_is_abi_aggregate(type))
	{
		// Ignore empty unions / structs on non-win.
		if (!build_target.x86.is_win32_float_struct_abi && type_is_empty_union_struct(type, true))
		{
			return abi_arg_new(ABI_ARG_IGNORE);
		}

		bool needs_padding = false;

		if (x86_should_aggregate_use_direct(context, type, &needs_padding))
		{
			// Not in reg on MCU
			ABIArgInfo *info = abi_arg_new_direct_coerce(abi_type_new_int_bits(32));
			if (build_target.x86.is_mcu_api) return info;
			return abi_arg_by_reg_attr(info);
		}

		// Pass over-aligned aggregates on Windows indirectly. This behavior was
		// added in MSVC 2015.
		if (build_target.x86.is_win32_float_struct_abi && type_abi_alignment(type) > 4)
		{
			return x86_create_indirect_result(context, type, BY_REG);
		}
		// Clang: Expand small (<= 128-bit) record types when we know that the stack layout
		// of those arguments will match the struct. This is important because the
		// LLVM backend isn't smart enough to remove byval, which inhibits many
		// optimizations.
		// Don't do this for the MCU if there are still free integer registers
		// (see X86_64 ABI for full explanation).
		if (size <= 128 / 8 && (!build_target.x86.is_mcu_api || !context->abi.int_registers) &&
				x86_can_expand_indirect_arg(type))
		{
			switch (context->abi.call_convention)
			{
				case CALL_CONVENTION_REGCALL:
				case CALL_CONVENTION_FAST:
				case CALL_CONVENTION_VECTOR:
					return abi_arg_new_expand_padded(type);
				default:
				{
					return abi_arg_new(ABI_ARG_EXPAND);
				}
			}
		}
		return x86_create_indirect_result(context, type, BY_NORMAL);
	}

	// Clang: On Darwin, some vectors are passed in memory, we handle this by passing
	// it as an i8/i16/i32/i64.
	if (type->type_kind == TYPE_VECTOR)
	{
		// On Windows, vectors are passed directly if registers are available, or
		// indirectly if not. This avoids the need to align argument memory. Pass
		// user-defined vector types larger than 512 bits indirectly for simplicity.
		if (build_target.x86.is_win32_float_struct_abi)
		{
			if (size < 512 / 8 && context->abi.sse_registers)
			{
				context->abi.sse_registers--;
				return abi_arg_by_reg_attr(abi_arg_new(ABI_ARG_DIRECT));
			}
			return x86_create_indirect_result(context, type, BY_REG);
		}
		if (build_target.x86.is_darwin_vector_abi)
		{
			if ((size == 1 || size == 2 || size == 4) || (size == 8 && type->vector.len == 1))
			{
				return abi_arg_new_direct_coerce(abi_type_new_int_bits(size * 8));
			}
		}
		if (x86_is_mmxtype(type))
		{
			return abi_arg_new_direct_coerce(abi_type_new_int_bits(64));
		}

		// Send as a normal parameter
		return abi_arg_new(ABI_ARG_DIRECT);
	}

	// We now handle primitives
	ByReg by_val = BY_NORMAL;
	if (x86_update_free_regs(context, type) && !build_target.x86.is_mcu_api)
	{
		switch (context->abi.call_convention)
		{
			case CALL_CONVENTION_FAST:
			case CALL_CONVENTION_VECTOR:
			case CALL_CONVENTION_REGCALL:
				if (size > 4) break;
				by_val = type_is_integer(type) || type_is_pointer(type) ? BY_REG : BY_NORMAL;
				break;
			default:
				by_val = BY_REG;
				break;
		}
	}

	if (type_is_promotable_integer(type))
	{
		ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
		if (by_val == BY_REG) info->attributes.by_reg = true;
		return info;
	}

	// i128 etc on stack.
	if (size > 8) x86_create_indirect_result(context, type, BY_REG);

	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT);
	if (by_val == BY_REG) info->attributes.by_reg = true;
	return info;
}


static void x86_compute_vector_args(GenContext *context, ABIArgInfo ***args_infos)
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
			if (build_target.x86.is_win32_float_struct_abi)
			{
				context->abi.sse_registers = 3;
			}
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

	signature->ret_abi_info = x86_classify_return(context, signature->rtype->type);

	if (context->abi.call_convention == CALL_CONVENTION_VECTOR)
	{
		FATAL_ERROR("X86 vector call not supported");
	}
	else
	{
		Decl **params = signature->params;
		VECEACH(params, i)
		{
			params[i]->var.abi_info = x86_classify_argument(context, params[i]->type);
		}
	}
}

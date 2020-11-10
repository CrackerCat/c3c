// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"


static CABIKind c_abi_get_kind(CallConvention convention)
{
	switch (build_target.arch)
	{
		case ARCH_TYPE_X86:
			switch (convention)
			{
				case CALL_CONVENTION_NORMAL:
					return build_target.os == OS_TYPE_WIN32 ? WIN32_CDECL : UNIX_CDECL;
				case CALL_CONVENTION_VECTOR:
					return build_target.os == OS_TYPE_WIN32 ? WIN32_VEC : UNIX_CDECL;
				case CALL_CONVENTION_STD:
					return build_target.os == OS_TYPE_WIN32 ? WIN32_STD : UNIX_CDECL;
				case CALL_CONVENTION_SYSCALL:
					return build_target.os == OS_TYPE_WIN32 ? WIN32_SYS : UNIX_CDECL;
				// Thiscall and pascal not supported.
				case CALL_CONVENTION_REGCALL:
					TODO
					break;
				case CALL_CONVENTION_FAST:
					TODO
					break;
			}
			UNREACHABLE
		case ARCH_TYPE_X86_64:
			switch (convention)
			{
				case CALL_CONVENTION_NORMAL:
				case CALL_CONVENTION_STD:
				case CALL_CONVENTION_SYSCALL:
				case CALL_CONVENTION_VECTOR:
					return build_target.os == OS_TYPE_WIN32 ? WIN64_CC : AMD64_CC;
				case CALL_CONVENTION_REGCALL:
					TODO
					break;
				case CALL_CONVENTION_FAST:
					TODO
					break;
			}
			UNREACHABLE
		default:
			TODO

	}
	TODO
}

typedef enum
{
	CABI_PARAM_SRETURN,
	CABI_PARAM_MEMORY,
} CABIParamKind;
typedef struct
{
	int index;
	CABIParamKind kind;
} CABIParam;


typedef struct
{
	CABIParam return_value;
	CABIParam failure;
} CallDescriptor;

ABIArgInfo *abi_arg_new(ABIKind kind)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = kind;
	return info;
}

AbiType *abi_type_new_plain(Type *type)
{
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_PLAIN;
	abi_type->type = type;
	return abi_type;
}

AbiType *abi_type_new_int_bits(unsigned bits)
{
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_INT_BITS;
	abi_type->int_bits = bits;
	return abi_type;
}

bool abi_type_is_integer(AbiType *type)
{
	return type->kind == ABI_TYPE_INT_BITS || type_is_integer(type->type);
}

bool abi_type_is_float(AbiType *type)
{
	return type->kind != ABI_TYPE_INT_BITS && type_is_float(type->type);
}

bool abi_type_size(AbiType *type)
{
	switch (type->kind)
	{
		case ABI_TYPE_INT_BITS:
			return type->int_bits / 8;
		case ABI_TYPE_PLAIN:
			return type_size(type->type);
	}
	UNREACHABLE;
}

bool abi_type_abi_alignment(AbiType *type)
{
	printf(">>>> %d\n", next_highest_power_of_2(24));
	switch (type->kind)
	{
		case ABI_TYPE_INT_BITS:
			return type_abi_alignment(type_unsigned_int_by_bitsize(next_highest_power_of_2(type->int_bits)));
		case ABI_TYPE_PLAIN:
			return type_abi_alignment(type->type);
	}
	UNREACHABLE;
}

bool abi_arg_is_indirect(ABIArgInfo *info)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_DIRECT_HVA:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_INT_EXTEND:
		case ABI_ARG_EXPAND:
		case ABI_ARG_EXPAND_PADDED:
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_DIRECT_HIGH:
			return false;
		case ABI_ARG_INDIRECT:
		case ABI_ARG_INDIRECT_REALIGNED:
			return true;
	}
}

ABIArgInfo *abi_arg_new_indirect_realigned(unsigned alignment)
{
	assert(alignment > 0);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT_REALIGNED);
	info->realignment = alignment;
	return info;
}

size_t expanded_size(ABIArgInfo *type_info, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return expanded_size(type_info, type->canonical);
		case TYPE_ARRAY:
			return expanded_size(type_info, type->array.base) * type->array.len;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			size_t result = 0;
			VECEACH(members, i)
			{
				members += expanded_size(type_info, members[i]->type);
			}
			return result;
		}
		case TYPE_UNION:
		{
			size_t largest = 0;
			Type *largest_type = NULL;
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				if (type_size(type) > largest)
				{
					largest = type_size(type);
					type = type->canonical;
				}
			}
			if (!largest) return 0;
			return expanded_size(type_info, type);
		}
		// Type complex: return 2;
		default:
			return 1;
	}
}


ABIArgInfo *abi_arg_new_direct_high(AbiType *high_type)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_HIGH);
	arg_info->partial_type = high_type;
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct_pair(AbiType *low_type, AbiType *high_type, AbiType *low_extend)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_PAIR);
	arg_info->direct_pair.low_extend_type = low_extend;
	arg_info->direct_pair.high_type = high_type;
	arg_info->direct_pair.low_type = low_type;
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct_coerce(AbiType *target_type)
{
	assert(target_type);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE);
	info->direct_coerce_type = target_type;
	return info;
}

ABIArgInfo *abi_arg_new_expand_padded(Type *type)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_EXPAND_PADDED);
	info->padding_type = type;
	return info;
}


ABIArgInfo *classify_return_type_default(Type *type)
{
	if (type == type_void)
	{
		return abi_arg_new(ABI_ARG_IGNORE);
	}

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type))
	{
		return abi_arg_new(ABI_ARG_INDIRECT);
	}

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_integer(type_lowering(type)))
	{
		return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
	}

	// No, then do a direct pass.
	return abi_arg_new(ABI_ARG_DIRECT);
}

static LLVMValueRef llvm_enter_struct_pointer_for_coerced_access(GenContext *context, LLVMValueRef src_ptr, LLVMTypeRef *source_type, size_t dest_size)
{
	// We do not enter a zero element struct.
	if (!LLVMCountStructElementTypes(*source_type)) return src_ptr;

	LLVMTypeRef first_element_type = LLVMStructGetTypeAtIndex(*source_type, 0);
	// Check if the first element
	size_t first_element_size = LLVMStoreSizeOfType(target_data_layout(), first_element_type);
	if (first_element_size < dest_size && first_element_size < LLVMStoreSizeOfType(target_data_layout(), *source_type))
	{
		// The first element is too small, so return the current pointer:
		return src_ptr;
	}
	// GEP into the first element.
	src_ptr = LLVMBuildStructGEP2(context->builder, first_element_type, src_ptr, 0, "dive");
	*source_type = first_element_type;
	// Recurse if it is a struct.
	if (LLVMGetTypeKind(first_element_type) == LLVMStructTypeKind)
	{
		return llvm_enter_struct_pointer_for_coerced_access(context, src_ptr, source_type, dest_size);
	}
	return src_ptr;
}


void c_abi_func_create(GenContext *context, FunctionSignature *signature)
{
	switch (build_target.abi)
	{
		case ABI_X64:
			c_abi_func_create_x64(context, signature);
			break;
		case ABI_X86:
		default:
			c_abi_func_create_x86(context, signature);
			break;
	}
}


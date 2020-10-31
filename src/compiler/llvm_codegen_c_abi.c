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

ABIArgInfo *create_abi_arg_indirect(unsigned alignment, bool by_val, bool realign, Type *padding)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->indirect_alignment = alignment;
	info->by_val = by_val;
	info->realign = realign;
	info->padding_type = padding;
	info->kind = ABI_ARG_INDIRECT;
	return info;
}

ABIArgInfo *create_abi_expand(bool pad_in_reg, Type *padding)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->padding_type = padding;
	info->padding_in_reg = pad_in_reg;
	info->kind = ABI_ARG_EXPAND;
	return info;
}

ABIArgInfo *create_abi_arg_direct(Type *type, unsigned offset, Type *padding, bool may_flatten)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->coerce_to = type;
	info->padding_type = padding;
	info->direct_offset = offset;
	info->kind = ABI_ARG_DIRECT;
	info->may_flatten = may_flatten;
	return info;
}

ABIArgInfo *create_abi_arg_direct_in_reg(void)
{
	ABIArgInfo *info = create_abi_arg_direct(NULL, 0, NULL, true);
	info->in_reg = true;
	return info;
}

ABIArgInfo *create_abi_arg_extend(Type *type)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->coerce_to = type_is_signed(type) ? type_int : type_uint;
	info->direct_offset = 0;
	info->padding_type = NULL;
	info->kind = ABI_ARG_EXTEND;
	return info;
}

ABIArgInfo *create_abi_arg_extend_in_reg(Type *type)
{
	ABIArgInfo *info = create_abi_arg_extend(type);
	info->in_reg = true;
	return info;
}

ABIArgInfo *create_abi_arg_expand()
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = ABI_ARG_EXPAND;
	return info;
}

ABIArgInfo *create_abi_arg_ignore()
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = ABI_ARG_IGNORE;
	return info;
}


bool c_abi_is_homogenous_aggregate(Type *type)
{
	return false;
}

ABIArgInfo *classify_return_type_default(Type *type)
{
	if (type == type_void) return create_abi_arg_ignore();

	if (c_abi_is_homogenous_aggregate(type))
	{
		return create_abi_arg_indirect(type_abi_alignment(type), true, false, NULL);
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



void c_abi_func_create(GenContext *context, FunctionSignature *signature)
{
	/*
	switch (c_abi_get_kind(signature->convention))
	{
		case WIN32_CDECL:
			c_abi_func_create_x86(signature);
		case WIN64_VEC_CC:
	}*/
}

/*
static c_abi_analyze_return(GenContext *context, Type *type)
{
}

void c_abi_func_create(GenContext *context, FunctionSignature *signature)
{
	context->abi.args = 0;
	context->abi.int_registers = 0;
	context->abi.sse_registers = 0;
	context->abi.simd_registers = 0;
	CABIKind kind;
	c_abi_analyze_return(context, signature->rtype->type);
}
*/
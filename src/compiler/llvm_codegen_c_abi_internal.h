#pragma once
// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

typedef enum
{
	WIN32_CDECL,
	WIN32_VEC,
	WIN32_STD,
	WIN32_SYS,
	UNIX_CDECL,
	WIN64_CC,
	AMD64_CC,
} CABIKind;

typedef enum
{
	BY_REG,
	BY_NORMAL
} ByReg;


ABIArgInfo *abi_arg_new(ABIKind kind);
ABIArgInfo *abi_arg_new_direct_pair(AbiType *low_type, AbiType *high_type, AbiType *low_extend);
ABIArgInfo *abi_arg_new_direct_high(AbiType *high_type);

ABIArgInfo *abi_arg_new_direct_coerce(AbiType *target_type);
ABIArgInfo *abi_arg_new_expand_padded(Type *type);
ABIArgInfo *abi_arg_new_indirect_realigned(unsigned alignment);

AbiType *abi_type_new_plain(Type *type);
AbiType *abi_type_new_int_bits(unsigned bits);
bool abi_type_is_integer(AbiType *type);
bool abi_type_is_float(AbiType *type);
bool abi_type_abi_alignment(AbiType *type);
bool abi_type_size(AbiType *type);


static inline ABIArgInfo *abi_arg_by_reg_attr(ABIArgInfo *info)
{
	info->attributes.by_reg = true;
	return info;
}
void c_abi_func_create_x86(GenContext *context, FunctionSignature *signature);
void c_abi_func_create_x64(GenContext *context, FunctionSignature *signature);
bool abi_arg_is_indirect(ABIArgInfo *info);
size_t expanded_size(ABIArgInfo *type_info, Type *type);

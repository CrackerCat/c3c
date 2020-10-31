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
	ABI_ARG_DIRECT,
	ABI_ARG_EXTEND,
	ABI_ARG_INDIRECT,
	ABI_ARG_IGNORE,
	ABI_ARG_EXPAND,
	CoerceAndExpand,
	ABI_ARG_ALLOCA
}  ABIKind;

typedef struct
{
	ABIKind kind : 4;
	union
	{
		unsigned indirect_alignment;
		unsigned direct_offset;
	};
	bool by_val : 1;
	bool in_reg : 1;
	bool realign : 1;
	bool may_flatten : 1;
	bool padding_in_reg : 1;
	union
	{
		Type* padding_type;
		Type* coerce_to;
	};
} ABIArgInfo;

ABIArgInfo *create_abi_arg_indirect(unsigned alignment, bool by_val, bool realign, Type *padding);
ABIArgInfo *create_abi_arg_direct(Type *type, unsigned offset, Type *padding, bool may_flatten);
ABIArgInfo *create_abi_arg_direct_in_reg(void);
ABIArgInfo *create_abi_arg_extend(Type *type);
ABIArgInfo *create_abi_arg_extend_in_reg(Type *type);
ABIArgInfo *create_abi_arg_expand();
ABIArgInfo *create_abi_arg_ignore();
ABIArgInfo *create_abi_expand(bool pad_in_reg, Type *padding);

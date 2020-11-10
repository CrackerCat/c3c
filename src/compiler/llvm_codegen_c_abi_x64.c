// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"

typedef enum
{
	UNNAMED,
	NAMED
} NamedArgument;

typedef enum
{
	// No not change ordering.
	CLASS_NO_CLASS,
	CLASS_MEMORY,
	CLASS_INTEGER,
	CLASS_SSE,
	CLASS_SSEUP,
} X64Class;

static ABIArgInfo *x64_classify_argument_type(GenContext *context, Type *type, unsigned free_int_regs, unsigned *int_regs, unsigned *sse_regs, NamedArgument is_named);

ABIArgInfo *x64_indirect_return_result(GenContext *context, Type *type)
{
	if (type_is_abi_aggregate(type))
	{
		return abi_arg_new(ABI_ARG_INDIRECT);
	}
	type = type_lowering(type);
	if (type_is_promotable_integer(type))
	{
		return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
	}
	return abi_arg_new(ABI_ARG_DIRECT);
}
static size_t x64_native_vector_size_for_avx(void)
{
	switch (build_target.x64.avx_level)
	{
		case AVX_NONE:
			return 16;
		case AVX:
			return 32;
		case AVX_512:
			return 64;
	}
	UNREACHABLE
}


static bool x64_type_is_illegal_vector(Type *type)
{
	// Only check vectors.
	if (type->type_kind != TYPE_VECTOR) return false;
	unsigned size = type_size(type);
	// Less than 64 bits or larger than the avx native size => not allowed.
	if (size <= 8 || size > x64_native_vector_size_for_avx()) return true;
	// If we pass i128 in mem, then check for that.
	if (build_target.x64.pass_int128_vector_in_mem)
	{
		// Illegal if i128/u128
		TypeKind kind = type->vector.base->type_kind;
		return kind == TYPE_I128 || kind == TYPE_U128;
	}
	// Otherwise fine!
	return true;
}

ABIArgInfo *x64_indirect_result(GenContext *context, Type *type, unsigned free_int_regs)
{
	// If this is a scalar LLVM value then assume LLVM will pass it in the right
	// place naturally.
	//
	// This assumption is optimistic, as there could be free registers available
	// when we need to pass this argument in memory, and LLVM could try to pass
	// the argument in the free register. This does not seem to happen currently,
	// but this code would be much safer if we could mark the argument with
	// 'onstack'. See PR12193.
	type = type_lowering(type);
	if (!type_is_abi_aggregate(type) && !x64_type_is_illegal_vector(type))
	{
		if (type_is_promotable_integer(type))
		{
			return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
		}
		// No change, just put it on the stack.
		return abi_arg_new(ABI_ARG_DIRECT);
	}

	// The byval alignment
	unsigned align = type_abi_alignment(type);

	// Pass as arguments if there are no more free int regs
	// (if 'onstack' appears, change this code)
	if (!free_int_regs)
	{
		unsigned size = type_size(type);
		if (align == 8 && size <= 8)
		{
			return abi_arg_new_direct_coerce(abi_type_new_int_bits(size * 8));
		}
	}
	if (align < 8)
	{
		return abi_arg_new_indirect_realigned(8);
	}
	return abi_arg_new(ABI_ARG_INDIRECT);
}


ABIArgInfo *x64_classify_reg_call_struct_type_check(GenContext *context, Type *type, unsigned *int_regs, unsigned *sse_regs)
{
	assert(type->type_kind == TYPE_STRUCT);
	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *member_type = members[i]->type->canonical;
		ABIArgInfo *member_info;
		if (member_type->type_kind == TYPE_STRUCT)
		{
			member_info = x64_classify_reg_call_struct_type_check(context, member_type, int_regs, sse_regs);
		}
		else
		{
			unsigned temp_int_reg = 0;
			unsigned temp_sse_reg = 0;
			member_info = x64_classify_argument_type(context, member_type, (unsigned)-1, &temp_int_reg, &temp_sse_reg, NAMED);
			int_regs += temp_int_reg;
			sse_regs += temp_sse_reg;
		}
		if (abi_arg_is_indirect(member_info))
		{
			*sse_regs = 0;
			*int_regs = 0;
			return x64_indirect_return_result(context, type);
		}
	}
	// Check this!
	return abi_arg_new(ABI_ARG_DIRECT);
}

ABIArgInfo *x64_classify_reg_call_struct_type(GenContext *context, Type *return_type)
{
	unsigned int_regs = 0;
	unsigned sse_regs = 0;
	ABIArgInfo *info = x64_classify_reg_call_struct_type_check(context, return_type, &int_regs, &sse_regs);
	if (int_regs > context->abi.int_registers || sse_regs > context->abi.sse_registers)
	{
		return x64_indirect_return_result(context, return_type);
	}
	return info;
}

static void x64_classify(GenContext *context, Type *type, size_t offset_base, X64Class *lo_class, X64Class *hi_class, NamedArgument named);

static X64Class x64_merge(X64Class accum, X64Class field)
{
	// 1. Same => result
	// 2. no class + something => something
	// 3. mem + something => mem
	// 4. int + something => int
	// 6. SSE

	// Accum should never be memory (we should have returned) or
	assert(accum != CLASS_MEMORY);
	if (accum == field) return accum;

	// Swap
	if (accum > field)
	{
		X64Class temp = field;
		field = accum;
		accum = temp;
	}
	switch (accum)
	{
		case CLASS_NO_CLASS:
			return field;
		case CLASS_MEMORY:
			return CLASS_MEMORY;
		case CLASS_INTEGER:
			// Other can only be non MEM and non NO_CLASS
			return CLASS_INTEGER;
		case CLASS_SSEUP:
		case CLASS_SSE:
			// Other can only be SSE type
			return CLASS_SSE;
	}
}
void x64_classify_post_merge(size_t size, X64Class *lo_class, X64Class *hi_class)
{
	// If one is MEM => both is mem
	// If X87UP is not before X87 => mem
	// If size > 16 && first isn't SSE or any other is not SSEUP => mem
	// If SSEUP is not preceeded by SSE/SSEUP => convert to SSE.
	if (*hi_class == CLASS_MEMORY) goto DEFAULT_TO_MEMORY;
	if (size > 16 && (*lo_class != CLASS_SSE || *hi_class != CLASS_SSEUP)) goto DEFAULT_TO_MEMORY;
	if (*hi_class == CLASS_SSEUP && *lo_class != CLASS_SSE && *lo_class != CLASS_SSEUP)
	{
		// TODO check this
		*hi_class = CLASS_SSE;
	}
	return;

	DEFAULT_TO_MEMORY:
	*lo_class = CLASS_MEMORY;
}

void x64_classify_struct_union(GenContext *context, Type *type, size_t offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class, NamedArgument named_arg)
{
	size_t size = type_size(type);
	// 64 byte max.
	if (size > 64) return;

	// Re-classify
	*current = CLASS_NO_CLASS;
	bool is_union = type->type_kind == TYPE_UNION;

	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		Decl *member = members[i];
		size_t offset = offset_base + member->offset;
		// The only case a 256-bit or a 512-bit wide vector could be used is when
		// the struct contains a single 256-bit or 512-bit element. Early check
		// and fallback to memory.
		if (size > 16 &&
			((!is_union && size != type_size(member->type))
			|| size > x64_native_vector_size_for_avx()))
		{
			*lo_class = CLASS_MEMORY;
			x64_classify_post_merge(size, lo_class, hi_class);
			return;
		}
		// Not aligned?
		if (offset % type_abi_alignment(member->type))
		{
			*lo_class = CLASS_MEMORY;
			x64_classify_post_merge(size, lo_class, hi_class);
			return;
		}

		X64Class field_lo;
		X64Class field_hi;
		x64_classify(context, member->type, offset, &field_lo, &field_hi, named_arg);
		*lo_class = x64_merge(*lo_class, field_lo);
		*hi_class = x64_merge(*hi_class, field_hi);
		if (*lo_class == CLASS_MEMORY || *hi_class == CLASS_MEMORY) break;
	}

	x64_classify_post_merge(size, lo_class, hi_class);
}

void x64_classify_array(GenContext *context, Type *type, size_t offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class, NamedArgument named_arg)
{
	size_t size = type_size(type);
	Type *element = type->array.base;
	size_t element_size = type_size(element);
	// Bigger than 64 bytes => MEM
	if (size > 64) return;

	if (offset_base % type_abi_alignment(element))
	{
		*lo_class = CLASS_MEMORY;
		x64_classify_post_merge(size, lo_class, hi_class);
		return;
	}

	// Re-classify
	*current = CLASS_NO_CLASS;
	// The only case a 256-bit or a 512-bit wide vector could be used is when
	// the struct contains a single 256-bit or 512-bit element. Early check
	// and fallback to memory.
	if (size > 16 && (size != type_size(element) || size > x64_native_vector_size_for_avx()))
	{
		*lo_class = CLASS_MEMORY;
		return;
	}

	size_t offset = offset_base;
	for (size_t i = 0; i < type->array.len; i++)
	{
		X64Class field_lo;
		X64Class field_hi;
		x64_classify(context, element, offset, &field_lo, &field_hi, named_arg);
		offset_base += element_size;
		*lo_class = x64_merge(*lo_class, field_lo);
		*hi_class = x64_merge(*hi_class, field_hi);
		if (*lo_class == CLASS_MEMORY || *hi_class == CLASS_MEMORY) break;
	}
	x64_classify_post_merge(size, lo_class, hi_class);
	assert(*hi_class != CLASS_SSEUP || *lo_class == CLASS_SSE);
}

void x64_classify_vector(Type *type, size_t offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class,
                         NamedArgument named_arg)
{
	unsigned size = type_size(type);
	// Pass as int
	if (size == 1 || size == 2 || size == 4)
	{
		*current = CLASS_INTEGER;
		// Check boundary crossing
		size_t lo = offset_base / 8;
		size_t hi = (offset_base + size - 1) / 8;

		// If it crosses boundary, split it.
		if (hi != lo)
		{
			*hi_class = *lo_class;
		}
		return;
	}
	if (size == 8)
	{
		Type *element = type->vector.base;

		// 1 x double passed in memory (by gcc)
		if (element->type_kind == TYPE_F64) return;

		// 1 x long long is passed different on older clang and
		// gcc, we pick SSE which is the GCC and later Clang standard.
		*current = CLASS_SSE;
		// Split if crossing boundary.
		if (offset_base && offset_base != 8)
		{
			*hi_class = *lo_class;
		}
		return;
	}
	if (size == 16 || named_arg || size <= x64_native_vector_size_for_avx())
	{
		if (build_target.x64.pass_int128_vector_in_mem) return;

		*lo_class = CLASS_SSE;
		*hi_class = CLASS_SSEUP;
	}
	// Default pass by mem
}

void x64_classify_complex(Type *type, size_t offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class)
{
	Type *element = type->complex;
	size_t element_size = type_size(element);
	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			*current = CLASS_INTEGER;
			break;
		case TYPE_I128:
		case TYPE_U128:
			*lo_class = *hi_class = CLASS_INTEGER;
			break;
		case TYPE_F16:
			TODO
		case TYPE_F32:
			*current = CLASS_SSE;
		case TYPE_F64:
			*lo_class = *hi_class = CLASS_SSE;
		case TYPE_F128:
			*current = CLASS_MEMORY;
		default:
			UNREACHABLE
	}
	size_t real = offset_base / 8;
	size_t imag = (offset_base + element_size) / 8;
	// If it crosses boundary, split it.
	if (*hi_class == CLASS_NO_CLASS && real != imag)
	{
		*hi_class = *lo_class;
	}
}

Decl *x64_get_member_at_offset(Decl *decl, unsigned offset)
{
	if (type_size(decl->type) <= offset) return NULL;
	Decl **members = decl->strukt.members;
	Decl *last_match = NULL;
	VECEACH(members, i)
	{
		if (members[i]->offset > offset) break;
		last_match = members[i];
	}
	assert(last_match);
	return last_match;
}

static void x64_classify(GenContext *context, Type *type, size_t offset_base, X64Class *lo_class, X64Class *hi_class, NamedArgument named)
{
	*lo_class = CLASS_NO_CLASS;
	*hi_class = CLASS_NO_CLASS;
	X64Class *current = offset_base < 8 ? lo_class : hi_class;
	*current = CLASS_MEMORY;
	type = type_lowering(type);
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_ENUM:
		case TYPE_TYPEDEF:
		case TYPE_FXX:
		case TYPE_IXX:
		case TYPE_TYPEID:
		case TYPE_FUNC:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_VOID:
			*current = CLASS_NO_CLASS;
			break;
		case TYPE_I128:
		case TYPE_U128:
		case TYPE_ERR_UNION:
		case TYPE_SUBARRAY:
			*lo_class = CLASS_INTEGER;
			*hi_class = CLASS_INTEGER;
			break;
		case TYPE_BOOL:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_ERRTYPE:
			*current = CLASS_INTEGER;
			break;
		case TYPE_F16:
			TODO
		case TYPE_F32:
		case TYPE_F64:
			*current = CLASS_SSE;
			break;
		case TYPE_F128:
			*lo_class = CLASS_SSE;
			*hi_class = CLASS_SSEUP;
			break;
		case TYPE_VARARRAY:
		case TYPE_POINTER:
			*current = CLASS_INTEGER;
			break;
		case TYPE_STRUCT:
		case TYPE_UNION:
			x64_classify_struct_union(context, type, offset_base, current, lo_class, hi_class, named);
			break;
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
			x64_classify_array(context, type, offset_base, current, lo_class, hi_class, named);
			break;
		case TYPE_VECTOR:
			x64_classify_vector(type, offset_base, current, lo_class, hi_class, named);
			break;
		case TYPE_COMPLEX:
			x64_classify_complex(type, offset_base, current, lo_class, hi_class);
			break;
	}
}

bool x64_bits_contain_no_user_data(Type *type, unsigned start, unsigned end)
{
	// If the bytes being queried are off the end of the type, there is no user
	// data hiding here.  This handles analysis of builtins, vectors and other
	// types that don't contain interesting padding.
	size_t size = type_size(type);
	if (size <= start) return true;
	if (type->type_kind == TYPE_ARRAY)
	{
		// Check each element to see if the element overlaps with the queried range.
		size_t element_size = type_size(type->array.base);
		for (unsigned i = 0; i < type->array.len; i++)
		{
			// If the element is after the span we care about, then we're done..
			size_t offset = i * element_size;
			if (offset >= end) break;
			unsigned element_start = offset < start ? start - offset : 0;
			if (!x64_bits_contain_no_user_data(type->array.base, element_start, end - offset)) return false;
		}
		// No overlap
		return true;
	}
	if (type_is_union_struct(type))
	{
		Decl **members = type->decl->strukt.members;
		VECEACH(members, i)
		{
			Decl *member = members[i];
			unsigned offset = member->offset;
			if (offset > end) break;
			unsigned field_start = offset < start ? start - offset : 0;
			if (!x64_bits_contain_no_user_data(member->type, field_start, end - offset)) return false;
		}
		// No overlap
		return true;
	}
	return false;
}

bool x64_contains_float_at_offset(Type *type, unsigned offset)
{
	if (offset == 0 && type->type_kind == TYPE_F32) return true;

	// If this is a struct, recurse into the field at the specified offset.
	if (type->type_kind == TYPE_STRUCT)
	{
		Decl *member = x64_get_member_at_offset(type->decl, offset);
		offset -= member->offset;
		return x64_contains_float_at_offset(member->type, offset);
	}
	if (type->type_kind == TYPE_ARRAY)
	{
		Type *element_type = type->array.base;
		unsigned element_size = type_size(element_type);
		offset -= (offset / element_size) * element_size;
		return x64_contains_float_at_offset(element_type, offset);
	}
	return false;
}

AbiType *x64_get_sse_type_at_offset(Type *type, unsigned ir_offset, Type *source_type, unsigned source_offset)
{
	// The only three choices we have are either double, <2 x float>, or float. We
	// pass as float if the last 4 bytes is just padding.  This happens for
	// structs that contain 3 floats.
	if (x64_bits_contain_no_user_data(source_type, source_offset + 4, source_offset + 8)) return abi_type_new_plain(type_float);

	// We want to pass as <2 x float> if the LLVM IR type contains a float at
	// offset+0 and offset+4.  Walk the LLVM IR type to find out if this is the
	// case.
	if (x64_contains_float_at_offset(type, ir_offset) &&
	    x64_contains_float_at_offset(type, ir_offset + 4))
	{
		return abi_type_new_plain(type_get_vector(type_float, 2));
	}
	return abi_type_new_plain(type_double);
}


AbiType *x64_get_int_type_at_offset(Type *type, unsigned offset, Type *source_type, unsigned source_offset)
{
	if (!offset)
	{
		switch (type->type_kind)
		{
			case TYPE_U64:
			case TYPE_I64:
			case TYPE_VARARRAY:
			case TYPE_POINTER:
				return abi_type_new_plain(type);
			case TYPE_BOOL:
			case TYPE_U8:
			case TYPE_I8:
			case TYPE_I16:
			case TYPE_U16:
			case TYPE_U32:
			case TYPE_I32:
				if (x64_bits_contain_no_user_data(source_type,
				                                  source_offset + type_size(type),
				                                  source_offset + 8))
				{
					return abi_type_new_plain(type);
				}
				break;
			case TYPE_STRUCT:
			{
				Decl *member = x64_get_member_at_offset(type->decl, offset);
				if (member)
				{
					return x64_get_int_type_at_offset(member->type, offset - member->offset, source_type, source_offset);
				}
				break;
			}
			case TYPE_ARRAY:
			{
				Type *element = type->array.base;
				size_t element_size = type_size(element);
				size_t element_offset = (offset / element_size) * element_size;
				return x64_get_int_type_at_offset(element, offset - element_offset, source_type, source_offset);
			}
			default:
				break;
		}
	}
	size_t size = type_size(source_type);
	assert(size != source_offset);
	if (size - source_offset > 8) return abi_type_new_plain(type_ulong);
	return abi_type_new_int_bits((size - source_offset) * 8);
}


static AbiType *x64_get_byte_vector_type(Type *type)
{
	// Wrapper structs/arrays that only contain vectors are passed just like
	// vectors; strip them off if present.
	Type *inner_type = type_find_single_struct_element(type);
	if (inner_type) type = inner_type;
	type = type_lowering(type);

	if (type->type_kind == TYPE_VECTOR)
	{
		Type *element = type->vector.base->canonical;
		if (build_target.x64.pass_int128_vector_in_mem && type_is_int128(element))
		{
			element = type_is_signed(element) ? type_long : type_ulong;
			// Convert to i64/u64
			return abi_type_new_plain(type_get_vector(element, type_size(type) / 8));
		}
		return abi_type_new_plain(type);
	}

	if (type->type_kind == TYPE_F128) return abi_type_new_plain(type);

	unsigned size = type_size(type);

	assert(size == 16 || size == 32 || size == 64);

	// Return a vector type based on the size.
	return abi_type_new_plain(type_get_vector(type_double, size / 8));
}

static ABIArgInfo *x64_get_argument_pair_return(AbiType *low_type, AbiType *high_type)
{
	// In order to correctly satisfy the ABI, we need to the high part to start
	// at offset 8.  If the high and low parts we inferred are both 4-byte types
	// (e.g. i32 and i32) then the resultant struct type ({i32,i32}) won't have
	// the second element at offset 8.  Check for this:
	unsigned low_size = abi_type_size(low_type);
	unsigned hi_start = aligned_offset(low_size, abi_type_abi_alignment(high_type));

	assert(hi_start != 0 && hi_start <= 8);
	Type *low_extend = NULL;
	// To handle this, we have to increase the size of the low part so that the
	// second element will start at an 8 byte offset.  We can't increase the size
	// of the second element because it might make us access off the end of the
	// struct.
	if (hi_start != 8)
	{
		if (abi_type_is_float(low_type))
		{
			low_extend = type_double;
		}
		else
		{
			low_extend = type_ulong;
		}
	}
	return abi_arg_new_direct_pair(low_type, high_type, abi_type_new_plain(low_extend));
}

ABIArgInfo *x64_classify_return(GenContext *context, Type *return_type)
{
	bool is_reg_call = context->abi.call_convention == CALL_CONVENTION_REGCALL;
	if (is_reg_call)
	{
		if (return_type->type_kind == TYPE_STRUCT)
		{
			return x64_classify_reg_call_struct_type(context, return_type);
		}
		// Complex > double => pass by memory
		if (return_type->type_kind == TYPE_COMPLEX && type_size(return_type->complex) > 8)
		{
			return x64_indirect_return_result(context, return_type);
		}
	}
	// AMD64-ABI 3.2.3p4: Rule 1. Classify the return type with the
	// classification algorithm.
	X64Class hi_class;
	X64Class lo_class;
	x64_classify(context, return_type, 0, &lo_class, &hi_class, NAMED);

	// Invariants
	assert(hi_class != CLASS_MEMORY || lo_class == CLASS_MEMORY);
	assert(hi_class != CLASS_SSEUP || lo_class == CLASS_SSE);

	AbiType *result_type = NULL;
	switch (lo_class)
	{
		case CLASS_NO_CLASS:
			if (hi_class == CLASS_NO_CLASS)
			{
				return abi_arg_new(ABI_ARG_IGNORE);
			}
			// If low part is padding, keep type null
			assert(hi_class == CLASS_SSE || hi_class == CLASS_INTEGER);
			break;
		case CLASS_SSEUP:
			UNREACHABLE
		case CLASS_MEMORY:
			// AMD64-ABI 3.2.3p4: Rule 2. Types of class memory are returned via
			// hidden argument.
			return x64_indirect_return_result(context, return_type);
		case CLASS_INTEGER:
			// AMD64-ABI 3.2.3p4: Rule 3. If the class is INTEGER, the next
			// available register of the sequence %rax, %rdx is used.
			result_type = x64_get_int_type_at_offset(return_type, 0, return_type, 0);
			if (hi_class == CLASS_NO_CLASS && abi_type_is_integer(result_type))
			{
				if (type_is_promotable_integer(return_type))
				{
					return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
				}
			}
			break;
		case CLASS_SSE:
			result_type = x64_get_sse_type_at_offset(return_type, 0, return_type, 0);
			break;
	}

	AbiType *high_part = NULL;
	switch (hi_class)
	{
		case CLASS_MEMORY:
		case CLASS_NO_CLASS:
			// Previously handled.
			break;
		case CLASS_INTEGER:
			high_part = x64_get_int_type_at_offset(return_type, 8, return_type, 8);
			// Return directly into high part.
			if (lo_class == CLASS_NO_CLASS) return abi_arg_new_direct_high(high_part);
			break;
		case CLASS_SSE:
			high_part = x64_get_sse_type_at_offset(return_type, 8, return_type, 8);
			if (lo_class == CLASS_NO_CLASS) return abi_arg_new_direct_high(high_part);
		case CLASS_SSEUP:
			// AMD64-ABI 3.2.3p4: Rule 5. If the class is SSEUP, the eightbyte
			// is passed in the next available eightbyte chunk if the last used
			// vector register.
			//
			// SSEUP should always be preceded by SSE, just widen.
			assert(lo_class == CLASS_SSE && "Unexpected SSEUp classification.");
			result_type = x64_get_byte_vector_type(return_type);
			break;
	}

	// If a high part was specified, merge it together with the low part.  It is
	// known to pass in the high eightbyte of the result.  We do this by forming a
	// first class struct aggregate with the high and low part: {low, high}
	if (high_part) return x64_get_argument_pair_return(result_type, high_part);

	if (return_type->type_kind == ABI_TYPE_PLAIN &&
		return_type->canonical == result_type->type->canonical)
	{
		return abi_arg_new(ABI_ARG_DIRECT);
	}
	return abi_arg_new_direct_coerce(result_type);
}

static ABIArgInfo *x64_classify_argument_type(GenContext *context, Type *type, unsigned free_int_regs, unsigned *int_regs, unsigned *sse_regs, NamedArgument is_named)
{
	bool is_reg_call = context->abi.call_convention == CALL_CONVENTION_REGCALL;
	if (is_reg_call)
	{
		if (type->type_kind == TYPE_STRUCT)
		{
			return x64_classify_reg_call_struct_type(context, type);
		}
	}
	X64Class hi_class;
	X64Class lo_class;
	x64_classify(context, type, 0, &lo_class, &hi_class, NAMED);

	// Invariants
	assert(hi_class != CLASS_MEMORY || lo_class == CLASS_MEMORY);
	assert(hi_class != CLASS_SSEUP || lo_class == CLASS_SSE);

	AbiType *result_type = NULL;
	*int_regs = 0;
	*sse_regs = 0;

	switch (lo_class)
	{
		case CLASS_NO_CLASS:
			if (hi_class == CLASS_NO_CLASS)
			{
				return abi_arg_new(ABI_ARG_IGNORE);
			}
			// If low part is padding, keep type null
			assert(hi_class == CLASS_SSE || hi_class == CLASS_INTEGER);
			break;
		case CLASS_SSEUP:
			UNREACHABLE
		case CLASS_MEMORY:
			return x64_indirect_result(context, type, free_int_regs);
		case CLASS_INTEGER:
			(*int_regs)++;
			result_type = x64_get_int_type_at_offset(type, 0, type, 0);
			if (hi_class == CLASS_NO_CLASS && abi_type_is_integer(result_type))
			{
				if (type_is_promotable_integer(type))
				{
					return abi_arg_new(ABI_ARG_DIRECT_INT_EXTEND);
				}
			}
			break;
		case CLASS_SSE:
			result_type = x64_get_sse_type_at_offset(type, 0, type, 0);
			(*sse_regs)++;
			break;
	}

	AbiType *high_part = NULL;
	switch (hi_class)
	{
		case CLASS_MEMORY:
			UNREACHABLE
		case CLASS_NO_CLASS:
			// Previously handled.
			break;
		case CLASS_INTEGER:
			(*int_regs)++;
			high_part = x64_get_int_type_at_offset(type, 8, type, 8);
			// Return directly into high part.
			if (lo_class == CLASS_NO_CLASS) return abi_arg_new_direct_high(high_part);
			break;
		case CLASS_SSE:
			high_part = x64_get_sse_type_at_offset(type, 8, type, 8);
			if (lo_class == CLASS_NO_CLASS) return abi_arg_new_direct_high(high_part);
			(*sse_regs)++;
			break;
		case CLASS_SSEUP:
			assert(lo_class == CLASS_SSE && "Unexpected SSEUp classification.");
			result_type = x64_get_byte_vector_type(type);
			break;
	}

	// If a high part was specified, merge it together with the low part.  It is
	// known to pass in the high eightbyte of the result.  We do this by forming a
	// first class struct aggregate with the high and low part: {low, high}
	if (high_part) return x64_get_argument_pair_return(result_type, high_part);

	return abi_arg_new_direct_coerce(result_type);
}


void c_abi_func_create_x64(GenContext *context, FunctionSignature *signature)
{
	// TODO 32 bit pointers
	// TODO allow override to get win64
	context->abi.call_convention = signature->convention;
	context->abi.sse_registers = 0;
	switch (signature->convention)
	{
		case CALL_CONVENTION_NORMAL:
		case CALL_CONVENTION_SYSCALL:
		case CALL_CONVENTION_VECTOR:
		case CALL_CONVENTION_FAST:
			context->abi.int_registers = 6;
			context->abi.sse_registers = 8;
			break;
		case CALL_CONVENTION_REGCALL:
			context->abi.int_registers = 11;
			context->abi.sse_registers = 16;
			break;
		default:
			// TODO
			UNREACHABLE
	}
	signature->ret_abi_info = x64_classify_return(context, signature->rtype->type);
	if (abi_arg_is_indirect(signature->ret_abi_info))
	{
		context->abi.int_registers--;
	}

	Decl **params = signature->params;
	VECEACH(params, i)
	{
		NamedArgument arg = NAMED;
		unsigned int_regs = 0;
		unsigned sse_regs = 0;
		params[i]->var.abi_info = x64_classify_argument_type(context, params[i]->type, context->abi.int_registers, &int_regs, &sse_regs, arg);
		if (int_regs > context->abi.int_registers || sse_regs > context->abi.sse_registers)
		{
			params[i]->var.abi_info = x64_indirect_result(context, params[i]->type, context->abi.int_registers);
			continue;
		}
		context->abi.int_registers -= int_regs;
		context->abi.sse_registers -= sse_regs;
	}
}

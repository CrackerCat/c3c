// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

LLVMTypeRef llvm_get_type(GenContext *context, Type *any_type);

static inline LLVMTypeRef llvm_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMTypeRef params[MAX_PARAMS];
	switch (decl->decl_kind)
	{
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_POISONED:
		case NON_TYPE_DECLS:
			UNREACHABLE
		case DECL_FUNC:
		{
			VECEACH(decl->func.function_signature.params, i)
			{
				params[i] = llvm_get_type(context, decl->func.function_signature.params[i]->type);
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			return LLVMFunctionType(llvm_get_type(context, decl->func.function_signature.rtype->type),
			                        params,
			                        param_size,
			                        decl->func.function_signature.variadic);

		}
		case DECL_TYPEDEF:
			return llvm_get_type(context, decl->typedef_decl.type_info->type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			LLVMTypeRef type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = type;
			VECEACH(decl->strukt.members, i)
			{
				vec_add(types, llvm_get_type(context, decl->strukt.members[i]->type));
			}
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			Decl *max_type = NULL;
			unsigned long long max_size = 0;
			LLVMTypeRef type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = type;
			VECEACH(decl->strukt.members, i)
			{
				Decl *member = decl->strukt.members[i];
				unsigned size = type_size(member->type);
				if (size > max_size || !max_type)
				{
					max_size = size;
					max_type = member;
				}
			}
			if (max_type)
			{
				LLVMTypeRef type_ref = llvm_get_type(context, max_type->type);
				LLVMStructSetBody(type, &type_ref, 1, false);
			}
			else
			{
				LLVMStructSetBody(type, NULL, 0, true);
			}
			return type;
		}
		case DECL_ENUM:
			return llvm_get_type(context, decl->type);
		case DECL_ERR:
		{
			LLVMTypeRef err_type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = err_type;
			LLVMTypeRef *types = NULL;
			vec_add(types, llvm_get_type(context, type_typeid));
			unsigned size = type_size(type_typeid);
			VECEACH(decl->strukt.members, i)
			{
				Type *type = decl->strukt.members[i]->type->canonical;
				unsigned alignment = type_abi_alignment(type);
				if (size % alignment != 0)
				{
					size += alignment - size % alignment;
				}
				size += type_size(type);
				vec_add(types, llvm_get_type(context, type));
			}
			unsigned padding = type_size(type_error) - size;
			if (padding > 0)
			{
				vec_add(types, LLVMIntTypeInContext(context->context, padding * 8));
			}
			LLVMStructSetBody(err_type, types, vec_size(types), false);
			return err_type;
		}
	}
	UNREACHABLE
}
static inline LLVMTypeRef llvm_type_from_ptr(GenContext *context, Type *type)
{
	LLVMTypeRef base_llvm_type = llvm_get_type(context, type->pointer);

	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	return type->backend_type = LLVMPointerType(base_llvm_type, /** TODO **/0);
}

static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	LLVMTypeRef base_llvm_type = llvm_get_type(context, type->array.base);
	return type->backend_type = LLVMArrayType(base_llvm_type, type->array.len);
}


static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (size_t i = type->array.len; i > 0; i--)
			{
				param_expand(context, params_ref, type->array.base);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				param_expand(context, params_ref, members[i]->type);
			}
			return;
		}
		case TYPE_ENUM:
			param_expand(context, params_ref, type_lowering(type));
			return;
		case TYPE_ERR_UNION:
			param_expand(context, params_ref, type_usize->canonical);
			param_expand(context, params_ref, type_usize->canonical);
			return;
		case TYPE_ERRTYPE:
			param_expand(context, params_ref, type_usize->canonical);
			return;
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
			if (!largest) return;
			param_expand(context, params_ref, largest_type);
			return;
		}
		default:
			// Type complex: return 2;
			vec_add(*params_ref, llvm_type(type));
			return;
	}

}
LLVMTypeRef llvm_func_type(GenContext *context, Type *type)
{
	LLVMTypeRef *params = NULL;
	FunctionSignature *signature = type->func.signature;

	c_abi_func_create(context, signature);

	LLVMTypeRef return_type = NULL;
	ABIArgInfo *reg_arg_info = signature->ret_abi_info;
	switch (reg_arg_info->kind)
	{
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_DIRECT_HIGH:
			TODO
		case ABI_ARG_EXPAND:
		case ABI_ARG_EXPAND_PADDED:
			UNREACHABLE;
		case ABI_ARG_DIRECT_INT_EXTEND:
		case ABI_ARG_DIRECT:
			return_type = llvm_type(signature->rtype->type);
			break;
		case ABI_ARG_INDIRECT:
		case ABI_ARG_INDIRECT_REALIGNED:
			vec_add(params, llvm_type(type_get_ptr(signature->rtype->type)));
			FALLTHROUGH;
		case ABI_ARG_IGNORE:
			return_type = llvm_type(type_void);
			break;
		case ABI_ARG_DIRECT_HVA:
			TODO
			break;
		case ABI_ARG_DIRECT_COERCE:
			return_type = llvm_abi_type(reg_arg_info->direct_coerce_type);
			break;
	}

	// Add in all of the required arguments.
	VECEACH(signature->params, i)
	{
		Decl *param = signature->params[i];
		ABIArgInfo *arg_info = param->var.abi_info;
		arg_info->param_index_start = vec_size(params);
		switch (arg_info->kind)
		{
			case ABI_ARG_IGNORE:
				break;
			case ABI_ARG_INDIRECT:
				vec_add(params, llvm_type(type_get_ptr(param->type)));
				break;
			case ABI_ARG_DIRECT_INT_EXTEND:
			case ABI_ARG_DIRECT:
				vec_add(params, llvm_type(param->type));
				break;
			case ABI_ARG_EXPAND:
				// Expanding a structs
				param_expand(context, &params, param->type->canonical);
				break;
			case ABI_ARG_DIRECT_PAIR:
				vec_add(params, llvm_abi_type(arg_info->direct_pair.low_type));
				vec_add(params, llvm_abi_type(arg_info->direct_pair.high_type));
				break;
			case ABI_ARG_DIRECT_COERCE:
				vec_add(params, llvm_abi_type(arg_info->direct_coerce_type));
				break;
			case ABI_ARG_INDIRECT_REALIGNED:
				vec_add(params, llvm_type(type_get_ptr(param->type)));
				break;
			case ABI_ARG_DIRECT_HVA:
			case ABI_ARG_EXPAND_PADDED:
			case ABI_ARG_DIRECT_HIGH:
				TODO
		}
	}

	unsigned parameters = vec_size(params);
	//if (signature->return_param) parameters++;

	LLVMTypeRef ret_type;
	/*
	if (signature->failable)
	{
		TODO
		ret_type = llvm_get_type(context, type_error);
	}
	else
	{
		ret_type = signature->return_param
				? llvm_get_type(context, type_void)
				: llvm_get_type(context, type->func.signature->rtype->type);
	}*/
	return LLVMFunctionType(return_type, params, parameters, signature->variadic);
}


LLVMTypeRef llvm_get_type(GenContext *context, Type *any_type)
{
	if (any_type->backend_type && LLVMGetTypeContext(any_type->backend_type) == context->context)
	{
		return any_type->backend_type;
	}
	DEBUG_LOG("Generating type %s", any_type->name);
	switch (any_type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_TYPEID:
			return any_type->backend_type = LLVMIntTypeInContext(context->context, any_type->builtin.bitsize);
		case TYPE_TYPEDEF:
			return any_type->backend_type = llvm_get_type(context, any_type->canonical);
		case TYPE_ENUM:
			return any_type->backend_type = llvm_get_type(context, any_type->decl->enums.type_info->type->canonical);
		case TYPE_ERR_UNION:
			return any_type->backend_type = LLVMIntTypeInContext(context->context, any_type->builtin.bitsize);
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
			return any_type->backend_type = llvm_type_from_decl(context, any_type->decl);
		case TYPE_FUNC:
			return any_type->backend_type = llvm_func_type(context, any_type);
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(context->context);
		case TYPE_F64:
		case TYPE_FXX:
			return any_type->backend_type = LLVMDoubleTypeInContext(context->context);
		case TYPE_F16:
			return any_type->backend_type = LLVMHalfTypeInContext(context->context);
		case TYPE_F32:
			return any_type->backend_type = LLVMFloatTypeInContext(context->context);
		case TYPE_F128:
			return any_type->backend_type = LLVMFP128TypeInContext(context->context);
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			return any_type->backend_type = LLVMIntTypeInContext(context->context, any_type->builtin.bitsize);
		case TYPE_IXX:
			return any_type->backend_type = LLVMIntTypeInContext(context->context, 32U);
		case TYPE_BOOL:
			// TODO
			return any_type->backend_type = LLVMIntTypeInContext(context->context, 1U);
		case TYPE_POINTER:
			return any_type->backend_type = llvm_type_from_ptr(context, any_type);
		case TYPE_STRING:
			// TODO
			return any_type->backend_type = LLVMPointerType(llvm_get_type(context, type_char), 0);
		case TYPE_ARRAY:
			return any_type->backend_type = llvm_type_from_array(context, any_type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = llvm_get_type(context, type_get_ptr(any_type->array.base));
			LLVMTypeRef size_type = llvm_get_type(context, type_usize);
			LLVMTypeRef array_type = LLVMStructCreateNamed(context->context, any_type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return any_type->backend_type = array_type;
		}
		case TYPE_VARARRAY:
			return any_type->backend_type = llvm_get_type(context, type_get_ptr(any_type->array.base));
		case TYPE_VECTOR:
			return any_type->backend_type = LLVMVectorType(llvm_type(any_type->vector.base), any_type->vector.len);
		case TYPE_COMPLEX:
		{
			LLVMTypeRef types[2] = { llvm_type(any_type->complex), llvm_type(any_type->complex) };
			return any_type->backend_type = LLVMStructType(types, 2, false);
		}
	}
	UNREACHABLE;
}

LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type)
{
	// gencontext_get_debug_type(context, type);
	return llvm_get_type(context, type);
}

LLVMTypeRef gencontext_get_llvm_abi_type(GenContext *context, AbiType *type)
{
	switch (type->kind)
	{
		case ABI_TYPE_PLAIN:
			return gencontext_get_llvm_type(context, type->type);
		case ABI_TYPE_INT_BITS:
			return LLVMIntTypeInContext(context->context, type->int_bits);
	}
	UNREACHABLE
}

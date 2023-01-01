#include "compiler.h"

uint8_t* code;
uint8_t* codeptr;
uint8_t* code_end;

void* last = 0;

ByteBuff code_buff = {};
ByteBuff data_buff = {};

SymCell* symbols;
FnBuff functions;

RelocBuff code_reloc_buff = {};
uint64_t code_reloc_ptr = 0;
RelocBuff data_reloc_buff = {};
uint64_t data_reloc_ptr = 0;

void codebyte(uint8_t byte) {
	*ByteBuff_push(&code_buff, 1) = byte;
}

void datau64(uint64_t u64) {
	uint64_t* this = (uint64_t*)(ByteBuff_push(&data_buff, 8));
	*this = u64;
}

void databyte(uint8_t byte) {
	*ByteBuff_push(&data_buff, 1) = byte;
}

void relocate() {
	for(uint64_t i = code_reloc_ptr; i < code_reloc_buff.lgt; i++) {
		Reloc* r = &code_reloc_buff.data[i];
		uint64_t base = (uint64_t)((r->rdata ? data_buff.data : code));
		uint64_t offset = r->base;

		switch(r->size) {
			case Reloc64:
				*((uint64_t*)(&code_buff.data[r->src])) = (uint64_t)(!r->relative? base + offset : (offset - (r->src + 8)));
				break;
			case Reloc32:
				*((uint32_t*)(&code_buff.data[r->src])) = (uint32_t)(!r->relative? base + offset : (offset - (r->src + 4)));
				break;
			case Reloc16:
				*((uint16_t*)(&code_buff.data[r->src])) = (uint16_t)(!r->relative? base + offset : (offset - (r->src + 2)));
				break;
			case Reloc8:
				*((uint8_t*)(&code_buff.data[r->src])) = (uint8_t)(!r->relative? base + offset : (offset - (r->src + 1)));
				break;
			default:
				ERROR(L"Code relocation with incorrect size indicator %d\n", r->size);
				break;
		}
	}
	for(uint64_t i = data_reloc_ptr; i < data_reloc_buff.lgt; i++) {
		Reloc* r = &data_reloc_buff.data[i];
		uint64_t base = (uint64_t)((r->rdata ? data_buff.data : code));
		uint64_t offset = r->base;
		switch(r->size) {
			case Reloc64:
				*((uint64_t*)(&data_buff.data[r->src])) = (uint64_t)(!r->relative? base + offset : (offset - (r->src + 8)));
				break;
			case Reloc32:
				*((uint32_t*)(&data_buff.data[r->src])) = (uint32_t)(!r->relative? base + offset : (offset - (r->src + 4)));
				break;
			case Reloc16:
				*((uint16_t*)(&data_buff.data[r->src])) = (uint16_t)(!r->relative? base + offset : (offset - (r->src + 2)));
				break;
			case Reloc8:
				*((uint8_t*)(&data_buff.data[r->src])) = (uint8_t)(!r->relative? base + offset : (offset - (r->src + 1)));
				break;
			default:
				ERROR(L"Code relocation with incorrect size indicator %d\n", r->size);
				break;
		}
	}

	data_reloc_ptr = data_reloc_buff.lgt;
	code_reloc_ptr = code_reloc_buff.lgt;
}

void data_reloc(uint64_t src, uint64_t base, uint8_t rdata, RelocSize size, uint8_t relative) {
	Reloc* r = RelocBuff_push(&data_reloc_buff, 1);
	r->src = src;
	r->base = base;
	r->rdata = rdata;
	r->size = size;
	r->relative = relative;
}

void code_reloc(uint64_t src, uint64_t base, uint8_t rdata, RelocSize size, uint8_t relative) {
	Reloc* r = RelocBuff_push(&code_reloc_buff, 1);
	r->src = src;
	r->base = base;
	r->rdata = rdata;
	r->size = size;
	r->relative = relative;
}

void reset_reloc_ptrs() {
	code_reloc_ptr = 0;
	data_reloc_ptr = 0;
}

Symbol* find_symb(SymCell* cell, wchar_t* name) {
	while(cell) {
		for(ssize_t i = cell->buff.lgt-1; i >= 0; i--) {
			if(wcscmp(cell->buff.data[i].name, name) == 0) {
				return &cell->buff.data[i];
			}
		}
		cell = cell->prev;
	}
	return NULL;
}

FnRecord* find_fn(SymCell* cell, wchar_t* name) {
	while(cell) {
		for(ssize_t i = cell->buff.lgt-1; i >= 0; i--) {
			if(wcscmp(cell->buff.data[i].name, name) == 0) {
				Symbol* rec = &cell->buff.data[i];
				for(ssize_t j = functions.lgt-1; j >= 0; j--) {
					if(functions.data[j].loc == rec->loc) {
						return &functions.data[j];
					}
				}
			}
		}
		cell = cell->prev;
	}
	return NULL;
}

uint64_t compile_ast(AST* ast, uint8_t* eof);

void bind_args(SymCell* env, wchar_t* fn_name, AST* args) {
	uint8_t eof = 0;

	FnRecord* fn = find_fn(env, fn_name);
	if(!fn) {
		ERROR(L"Not a function: '%ls'\n", fn_name);
		return;
	}

	AST* arg = args;
	for(uint32_t i = 0; i < fn->n_args; i++) {
		if(!arg) {
			ERROR(L"Syntax error in call to '%ls': expected %d arguments, got %d\n", fn_name, fn->n_args, i+1);
			return;
		}

		compile_ast(arg, &eof);

		Symbol* rec = find_symb(env, (wchar_t*)fn->args[i]->name);
		codebyte(0x48);
		codebyte(0xa3);
		code_reloc(code_buff.lgt, rec->loc, 1, Reloc64, 0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		arg = arg->next;
	}
	if(arg) {
		ERROR(L"Syntax error in call to '%ls': expected at most %d arguments\n", fn_name, fn->n_args);
		return;
	}
}

uint64_t compile_ast(AST* ast, uint8_t* eof) {
	switch(ast->type) {
		case ast_none:
			last = 0;
			*eof = 1;
			return 0;
			break;
		case ast_def: {
			AST* var = ast->child0;
			AST* val = ast->child1;
			uint64_t loc = data_buff.lgt;

			switch(val->type) {
				case ast_num:
					compile_ast(val, eof);
					loc = data_buff.lgt;
					datau64((uint64_t)(val->data));
					codebyte(0x48);
					codebyte(0xa1);
					code_reloc(code_buff.lgt, loc, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					break;
				case ast_var: {
					uint64_t vloc = compile_ast(val, eof);

					loc = data_buff.lgt;
					datau64(0);
					codebyte(0x48);
					codebyte(0xa1);
					code_reloc(code_buff.lgt, loc, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					break;
				}
				case ast_str: {
						break;
					}
				case ast_call: {
					compile_ast(val, eof);
					loc = data_buff.lgt;
					datau64(0);
					codebyte(0x48);
					codebyte(0xa3);
					code_reloc(code_buff.lgt, loc, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					break;
				}
				case ast_fn:
					loc = compile_ast(val, eof);
					break;
			}

			Symbol* symb = SymBuff_push(&symbols->buff, 1);
			symb->name = calloc(sizeof(wchar_t), wcslen((wchar_t*)var->data)+1);
			wcscpy(symb->name, (wchar_t*)var->data);
			symb->loc = loc;
			return loc;
			break;
		}
		case ast_num:
			codebyte(0x48);
			codebyte(0xb8);
			codebyte((uint8_t)((uint64_t)ast->data & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 8) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 16) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 24) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 32) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 40) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 48) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 56) & 0xff));

			return (uint64_t)ast->data;
			break;
		case ast_var: {
			Symbol* symb = find_symb(symbols, (wchar_t*)ast->data);
			if(!symb) {
				ERROR(L"Variable not found: '%ls'\n", (wchar_t*)ast->data);
				return 0;
			}

			codebyte(0x48);
			codebyte(0xa3);
			code_reloc(code_buff.lgt, symb->loc, 1, Reloc64, 0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);

			return symb->loc;
		}
		case ast_fn: {
			SymCell* symb_start = symbols;

			SymCell* fn_env = calloc(sizeof(SymCell), 1);
			fn_env->prev = symbols;
			symbols = fn_env;

			AST* args = ast->child0;
			AST* arg = args;
			FnRecord* f = FnBuff_push(&functions, 1);

			f->env = fn_env;

			uint8_t n = 0;
			while(arg) {
				arg = arg->next;
				n++;
			}
			f->n_args = n;
			f->args = calloc(sizeof(Symbol), n);
			arg = args;
			n = 0;
			while(arg) {
				Symbol* this_arg = SymBuff_push(&symbols->buff, 1);
				ssize_t name_lgt = wcslen((wchar_t*)arg->data);
				this_arg->name = calloc(sizeof(wchar_t), name_lgt);
				wcscpy(this_arg->name, (wchar_t*)arg->data);
				this_arg->loc = data_buff.lgt;
				datau64(0);
				f->args[n] = this_arg;
				arg = arg->next;
				n++;
			}

			codebyte(0xe9);
			uint64_t rel = code_buff.lgt;
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			uint64_t fn_loc = code_buff.lgt;
			compile_ast(ast->child1, eof);
			codebyte(0xc3);

			f->loc = fn_loc;
			code_reloc(rel, code_buff.lgt, 0, Reloc32, 1);

			codebyte(0x48);
			codebyte(0xb8);
			rel = code_buff.lgt;
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			code_reloc(rel, fn_loc, 0, Reloc64, 0);

			symbols = symb_start;
			return fn_loc;
			break;
		}
		case ast_call: {
			SymCell* symb_start = symbols;
			Symbol* rec = find_symb(symbols, (wchar_t*)ast->child0->data);
			if(!rec) {
				last = 0;
				ERROR(L"Name not found: '%ls'\n", (wchar_t*)ast->child0->data);
				break;
			}
			FnRecord* fn_rec = find_fn(symbols, (wchar_t*)ast->child0->data);
			if(!fn_rec || !fn_rec->env) {
				last = 0;
				ERROR(L"Not a function: '%ls'\n", rec->name);
				break;
			}

			bind_args((SymCell*)fn_rec->env, rec->name, ast->child1);

			codebyte(0xe9);
			code_reloc(code_buff.lgt, rec->loc, 0, Reloc32, 1);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			symbols = symb_start;
			return 0;
			break;
		}
		default: {
			last = 0;
			ERROR(L"AST with unimplemented type %d\n", ast->type);
			return 0;
			break;
		}
	}
}

void compile(uint8_t* eof) {
	last = 0;

	AST* ast = try_rule(L"program");
	
	if(ast) {
		last = (void*)code_buff.lgt;
		compile_ast(ast, eof);
	} else {
		ERROR(L"Syntax error\n");
	}

	if(last) {
		codebyte(0xc3);
	}
}

void init_compiler() {
	symbols = calloc(sizeof(SymCell), 1);
}

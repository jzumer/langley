#include "compiler.h"

// New version uses Destination-Driven Code Generation

uint8_t* code;
uint8_t* codeptr;
uint8_t* code_end;

void* last = 0;

lab_t nolabel = lab_none;
loc_t effect = loc_discard;
loc_t reg = loc_reg;

LocBuff locs = {};

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

void datau32(uint64_t u32) {
	uint32_t* this = (uint32_t*)(ByteBuff_push(&data_buff, 4));
	*this = u32;
}

void databyte(uint8_t byte) {
	*ByteBuff_push(&data_buff, 1) = byte;
}

void relocate() {
	for(uint64_t i = code_reloc_ptr; i < code_reloc_buff.lgt; i++) {
		Reloc* r = &code_reloc_buff.data[i];
		uint64_t base = (uint64_t)((r->rdata ? data_buff.data : code) + r->base);
		uint64_t offset = r->base;

		switch(r->size) {
			case Reloc64:
				*((uint64_t*)(&code_buff.data[r->src])) = (uint64_t)(!r->relative? base : (offset - (r->src + 8)));
				break;
			case Reloc32:
				*((uint32_t*)(&code_buff.data[r->src])) = (uint32_t)(!r->relative? base : (offset - (r->src + 4)));
				break;
			case Reloc16:
				*((uint16_t*)(&code_buff.data[r->src])) = (uint16_t)(!r->relative? base : (offset - (r->src + 2)));
				break;
			case Reloc8:
				*((uint8_t*)(&code_buff.data[r->src])) = (uint8_t)(!r->relative? base : (offset - (r->src + 1)));
				break;
			default:
				ERROR(L"Code relocation with incorrect size indicator %d\n", r->size);
				break;
		}
	}
	for(uint64_t i = data_reloc_ptr; i < data_reloc_buff.lgt; i++) {
		Reloc* r = &data_reloc_buff.data[i];
		uint64_t base = (uint64_t)((r->rdata ? data_buff.data : code) + r->base);
		uint64_t offset = r->base;
		switch(r->size) {
			case Reloc64:
				*((uint64_t*)(&data_buff.data[r->src])) = (uint64_t)(!r->relative? base : (offset - (r->src + 8)));
				break;
			case Reloc32:
				*((uint32_t*)(&data_buff.data[r->src])) = (uint32_t)(!r->relative? base : (offset - (r->src + 4)));
				break;
			case Reloc16:
				*((uint16_t*)(&data_buff.data[r->src])) = (uint16_t)(!r->relative? base : (offset - (r->src + 2)));
				break;
			case Reloc8:
				*((uint8_t*)(&data_buff.data[r->src])) = (uint8_t)(!r->relative? base : (offset - (r->src + 1)));
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

uint8_t compile_ast(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1, uint8_t compile_next);

uint8_t current_register = 0;
uint8_t reg_used = 0;

uint8_t next_register() {
	current_register = (current_register+1) % 8;
	return (reg_used & (1 << current_register));
}

uint8_t reset_register() {
	current_register = 0;
	return (reg_used & 1);
}

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

		Symbol* rec = find_symb(env, (wchar_t*)fn->args[i]->name);
		compile_ast(arg, &rec->loc_type, &rec->loc, &nolabel, 0, 0, 0);
		arg = arg->next;
	}
	if(arg) {
		ERROR(L"Syntax error in call to '%ls': expected at most %d arguments\n", fn_name, fn->n_args);
		return;
	}
}

void compile_mov(loc_t loc_type_to, uint64_t loc_to, loc_t loc_type_from, uint64_t loc_from) {
	switch(loc_type_to) {
		case loc_reg:
			switch(loc_type_from) {
				case loc_reg:
					if(loc_from == loc_to) { break; }
					codebyte(0x48);
					codebyte(0x89);
					codebyte(0xc0 + loc_to + 8 * loc_from);
					break;
				case loc_mem: {
					uint64_t addr_reg = (loc_to + 1) % 8;
					codebyte(0x50 + addr_reg);

					codebyte(0x48);
					codebyte(0xb8 + addr_reg);
					code_reloc(code_buff.lgt, loc_from, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);

					codebyte(0x48);
					codebyte(0x8b);
					codebyte(0x00 + addr_reg + loc_to * 8);

					codebyte(0x58 + addr_reg);
					break;
				}
				default:
					break;
			}
			break;
		case loc_mem:
			switch(loc_type_from) {
				case loc_mem:
					if(loc_from == loc_to) { break; }
					codebyte(0x50);
					uint64_t addr_reg = 1;
					codebyte(0x50 + addr_reg);

					codebyte(0x48);
					codebyte(0xb8 + addr_reg);
					code_reloc(code_buff.lgt, loc_from, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);

					codebyte(0x48);
					codebyte(0x8b);
					codebyte(0x00 + addr_reg + 0 * 8);

					codebyte(0x48);
					codebyte(0xb8 + addr_reg);
					code_reloc(code_buff.lgt, loc_to, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);

					codebyte(0x48);
					codebyte(0x89);
					codebyte(0x00 + addr_reg + 0 * 8);

					codebyte(0x58 + addr_reg);

					codebyte(0x58);
					break;
				case loc_reg: {
					uint64_t addr_reg = (loc_from + 1) % 8;
					codebyte(0x50 + addr_reg);

					codebyte(0x48);
					codebyte(0xb8 + addr_reg);
					code_reloc(code_buff.lgt, loc_to, 1, Reloc64, 0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);
					codebyte(0);

					codebyte(0x48);
					codebyte(0x89);
					codebyte(0x00 + addr_reg + loc_from * 8);

					codebyte(0x58 + addr_reg);
					break;
				}
				default:
					break;
			}
			break;
		default:
			break;
	}
}

void compile_value_jmp(loc_t loc_type, uint64_t loc, lab_t lab_type, uint64_t* lab0, uint64_t* lab1) {
	if(lab_type != lab_none) {
		LocRecord* rec = LocBuff_push(&locs, 1);
		rec->lab_type = lab_cond;
		switch(loc_type) {
			case loc_reg:
				codebyte(0x48);
				codebyte(0x85);
				codebyte(0xc0 + loc + loc * 8);
				codebyte(0x0f);
				codebyte(0x84); // jz (rel32)
				*lab1 = code_buff.lgt;
				codebyte(0);
				codebyte(0);
				codebyte(0);
				codebyte(0);
				*lab0 = code_buff.lgt;
				break;
			case loc_mem:
				codebyte(0x48);
				codebyte(0x8b);
				codebyte(0x04);
				codebyte(0x25);
				codebyte((uint8_t)((uint64_t)loc & 0xff));
				codebyte((uint8_t)(((uint64_t)loc >> 8) & 0xff));
				codebyte((uint8_t)(((uint64_t)loc >> 16) & 0xff));
				codebyte((uint8_t)(((uint64_t)loc >> 24) & 0xff));

				codebyte(0x50);

				codebyte(0x48);
				codebyte(0x83);
				codebyte(0x38);
				codebyte(0x00);
				codebyte(0x58);

				codebyte(0x0f);
				codebyte(0x84); // jz
				*lab1 = code_buff.lgt;
				codebyte(0);
				codebyte(0);
				codebyte(0);
				codebyte(0);
				*lab0 = code_buff.lgt;
				break;
			default:
				LocBuff_pop(&locs, 1);
				break;
		}
	}
}

void compile_var(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	Symbol* symb = find_symb(symbols, (wchar_t*)ast->data);
	if(!symb) {
		ERROR(L"Variable not found: '%ls'\n", (wchar_t*)ast->data);
		return;
	}
	compile_mov(*loc_type, *loc, symb->loc_type, symb->loc);
	compile_value_jmp(*loc_type, *loc, *lab_type, lab0, lab1);
}

void compile_num(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	switch(*loc_type) {
		case loc_reg:
			codebyte(0x48);
			codebyte(0xb8 + *loc);
			codebyte((uint8_t)((uint64_t)ast->data & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 8) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 16) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 24) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 32) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 40) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 48) & 0xff));
			codebyte((uint8_t)(((uint64_t)ast->data >> 56) & 0xff));
			break;
		case loc_mem:
			//64-bit num can't be moved to mem in one op, need to use reg tmp
			codebyte(0x50);
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
			compile_mov(*loc_type, *loc, loc_reg, 0);
			codebyte(0x58);
			break;
		default:
			break;
	}

	compile_value_jmp(*loc_type, *loc, *lab_type, lab0, lab1);
}

void compile_fn(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
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
		this_arg->loc_type = loc_mem;
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
	// Return location in call is rax, so loc is loc_reg, 0
	uint64_t which_reg = 0;
	compile_ast(ast->child1, &reg, &which_reg, &nolabel, 0, 0, 1);
	codebyte(0xc3);

	f->loc = fn_loc;
	code_reloc(rel, code_buff.lgt, 0, Reloc32, 1);

	*loc_type = loc_mem;
	*loc = fn_loc;

	symbols = symb_start;
}

void compile_call(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	SymCell* symb_start = symbols;
	Symbol* rec = find_symb(symbols, (wchar_t*)ast->child0->data);
	if(!rec) {
		ERROR(L"Name not found: '%ls'\n", (wchar_t*)ast->child0->data);
	}
	FnRecord* fn_rec = find_fn(symbols, (wchar_t*)ast->child0->data);
	if(!fn_rec || !fn_rec->env) {
		ERROR(L"Not a function: '%ls'\n", rec->name);
	}

	bind_args((SymCell*)fn_rec->env, rec->name, ast->child1);

	if((rec->loc & 0xffffffff00000000) == 0) {
		codebyte(0xe8);
		code_reloc(code_buff.lgt, rec->loc, 0, Reloc32, 1);
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
	} else {
		codebyte(0x49);
		codebyte(0xba);
		for(uint8_t i = 0; i < 8; i++) {
			uint8_t off = (((uint64_t)rec->loc) >> (8 * i)) & 0xff;
			codebyte(off);
		}

		codebyte(0x41);
		codebyte(0xff);
		codebyte(0xd2);
	}

	// Move result from rax to the desired location
	compile_mov(*loc_type, *loc, loc_reg, 0);

	symbols = symb_start;
	return;
}

void compile_str(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	ssize_t lgt = ((uint64_t*)ast->data)[0] + 1;
	wchar_t* str = ((wchar_t*)ast->data) + (sizeof(uint64_t) / sizeof(wchar_t));

	uint64_t here = data_buff.lgt;
	datau64(lgt);
	for(ssize_t i = 0; i < lgt; i++) {
		datau32(str[i]); // XXX: we "know" that wchar_t is 4 bytes
	}
	datau32(0);

	switch(*loc_type) {
		case loc_mem:
			*loc = here;
			break;
		case loc_reg:
			codebyte(0x48);
			codebyte(0xb8 + *loc);
			code_reloc(code_buff.lgt, here, 1, Reloc64, 0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			codebyte(0);
			break;
		default:
			break;
	}
}

void compile_cond(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	AST* cond = ast->child0;
	AST* then = ast->child1;
	AST* otherwise = then->next;

	lab_t this_lab = lab_cond;
	uint64_t lab_true = 0;
	uint64_t lab_false = 0;

	compile_ast(cond, loc_type, loc, &this_lab, &lab_true, &lab_false, 1);

	compile_ast(then, loc_type, loc, &nolabel, lab0, lab1, 0);
	uint64_t end_loc = 0;
	if(otherwise) {
		codebyte(0xe9);
		end_loc = code_buff.lgt;
		codebyte(0);
		codebyte(0);
		codebyte(0);
		codebyte(0);
	}
	uint64_t brfalse = code_buff.lgt;
	if(otherwise) {
		compile_ast(otherwise, loc_type, loc, &nolabel, lab0, lab1, 1);
		code_reloc(end_loc, code_buff.lgt, 0, Reloc32, 1);
	}

	if(lab_false) {
		code_reloc(lab_false, brfalse, 0, Reloc32, 1);
	}
}

void compile_loop(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	uint64_t beg = code_buff.lgt;
	lab_t my_lab = lab_loop;
	uint64_t loc_start = locs.lgt;
	compile_ast(ast->child0, loc_type, loc, &my_lab, lab0, lab1, 1);
	codebyte(0xe9);
	uint64_t ret_jmp = code_buff.lgt;
	codebyte(0);
	codebyte(0);
	codebyte(0);
	codebyte(0);
	code_reloc(ret_jmp, beg, 0, Reloc32, 1);

	for(int64_t i = locs.lgt-1; i >= 0; i--) {
		if(locs.data[i].lab_type == lab_loop) {
			if(locs.data[i].lab1) {
				code_reloc(locs.data[i].lab1, code_buff.lgt, 0, Reloc32, 1);
			}
			if(locs.data[i].lab0) {
				code_reloc(locs.data[i].lab0, beg, 0, Reloc32, 1);
			}
		}
	}

	LocBuff_pop(&locs, locs.lgt - loc_start);
}

void compile_break(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	//if(*lab_type != lab_loop) {
	//	ERROR(L"Encountered break outisde loop\n");
	//}
	codebyte(0xe9);
	uint64_t ret_jmp = code_buff.lgt;
	codebyte(0);
	codebyte(0);
	codebyte(0);
	codebyte(0);

	LocRecord* rec = LocBuff_push(&locs, 1);
	rec->lab1 = ret_jmp;
	rec->lab0 = 0;
	rec->lab_type = lab_loop;
}

void compile_def(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1) {
	AST* var = ast->child0;
	AST* val = ast->child1;

	Symbol* symb = SymBuff_push(&symbols->buff, 1);
	symb->name = calloc(sizeof(wchar_t), wcslen((wchar_t*)var->data)+1);
	wcscpy(symb->name, (wchar_t*)var->data);

	loc_t sloc_type = loc_mem;
	uint64_t sloc = data_buff.lgt;
	datau64(0);

	if(val->type != ast_fn) {
		symb->loc_type = sloc_type;
		symb->loc = sloc; // TODO: use regs and check if occupied and spill if so
		// TODO 2: handle reg-mem 'dual location'
		compile_ast(val, &sloc_type, &sloc, &nolabel, lab0, lab1, 1);
	} else {
		compile_ast(val, &sloc_type, &sloc, &nolabel, lab0, lab1, 1);
		symb->loc_type = sloc_type;
		symb->loc = sloc;
	}

	compile_mov(*loc_type, *loc, sloc_type, sloc);
}

uint8_t compile_ast(AST* ast, loc_t* loc_type, uint64_t* loc, lab_t* lab_type, uint64_t* lab0, uint64_t* lab1, uint8_t compile_next) {
	switch(ast->type) {
		case ast_none:
			last = 0;
			return 0;
		case ast_str:
			compile_str(ast, loc_type, loc, lab_type, lab0, lab1);
			break;
		case ast_cond:
			compile_cond(ast, loc_type, loc, lab_type, lab0, lab1);
			break;
		case ast_loop:
			compile_loop(ast, loc_type, loc, lab_type, lab0, lab1);
			break;
		case ast_break:
			compile_break(ast, loc_type, loc, lab_type, lab0, lab1);
			break;
		case ast_def:
				compile_def(ast, loc_type, loc, lab_type, lab0, lab1);
				break;
		case ast_call:
				compile_call(ast, loc_type, loc, lab_type, lab0, lab1);
				break;
		case ast_fn:
				compile_fn(ast, loc_type, loc, lab_type, lab0, lab1);
				break;
		case ast_num:
				compile_num(ast, loc_type, loc, lab_type, lab0, lab1);
				break;
		case ast_var:
				compile_var(ast, loc_type, loc, lab_type, lab0, lab1);
				break;
		default:
				ERROR(L"NO!\n");
	}

	if(compile_next && ast->next) {
		return compile_ast(ast->next, loc_type, loc, lab_type, lab0, lab1, compile_next);
	}
	return 1;
}

void compile(uint8_t* eof) {
	last = 0;
	loc_t ret_loc = loc_reg;
	uint64_t ret_reg = 0;

	AST* ast = try_rule(L"program");
	
	if(ast) {
		last = (void*)code_buff.lgt;
		*eof = !compile_ast(ast, &ret_loc, &ret_reg, &nolabel, 0, 0, 1);
	} else {
		ERROR(L"Syntax error\n");
	}

	if(last) {
		codebyte(0xc3);
	}
}

void register_fn(wchar_t* name, void*(*fn)(), uint8_t n_args, wchar_t** args) {
	Symbol* symb = SymBuff_push(&symbols->buff, 1);
	symb->name = calloc(sizeof(wchar_t), wcslen(name));
	wcscpy(symb->name, name);
	loc_t sloc_type = loc_mem;
	symb->loc_type = loc_mem;
	symb->loc = (uint64_t)fn;

	SymCell* symb_start = symbols;

	SymCell* fn_env = calloc(sizeof(SymCell), 1);
	fn_env->prev = symbols;
	symbols = fn_env;

	FnRecord* f = FnBuff_push(&functions, 1);

	f->env = fn_env;

	f->n_args = n_args;
	f->args = calloc(sizeof(Symbol), n_args);
	for(ssize_t n = 0; n < n_args; n++) {
		Symbol* this_arg = SymBuff_push(&symbols->buff, 1);
		ssize_t name_lgt = wcslen(args[n]);
		this_arg->name = calloc(sizeof(wchar_t), name_lgt+1);
		wcscpy(this_arg->name, (wchar_t*)args[n]);
		this_arg->loc = data_buff.lgt;
		this_arg->loc_type = loc_mem;
		datau64(0);
		f->args[n] = this_arg;
	}

	f->loc = (uint64_t)fn;

	symbols = symb_start;
}

void modfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"%");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* rhs = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = lhs[0] % rhs[0];
}

void divfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"/");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* rhs = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = lhs[0] / rhs[0];
}

void mulfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"*");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* rhs = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = lhs[0] * rhs[0];
}

void subfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"-");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* rhs = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = lhs[0] - rhs[0];
}

void addfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"+");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* rhs = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = lhs[0] + rhs[0];
}

void printfn() {
	//register uint64_t what asm("rax");
	FnRecord* self = find_fn(symbols, L"print");
	uint8_t* what = data_buff.data + self->args[0]->loc;
	uint64_t lgt = ((uint64_t*)what)[0];
	wchar_t* str = (wchar_t*)what + sizeof(uint64_t) / sizeof(wchar_t);
	wprintf(str);
	wprintf(L"\n");
}

void printhexfn() {
	//register uint64_t what asm("rax");
	FnRecord* self = find_fn(symbols, L"printhex");
	uint64_t* what = (uint64_t*)(data_buff.data + self->args[0]->loc);
	wprintf(L"%llX\n", what[0]);
}

void reservefn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"reserve");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);

	uint64_t out = (uint64_t)calloc(lhs[0], 1);

	ret = out;
}

void reserve64fn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"reserve64");
	uint64_t* lhs = (uint64_t*)(data_buff.data + self->args[0]->loc);

	uint64_t out = (uint64_t)calloc(lhs[0], 8);

	ret = out;
}

void get64fn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"get64");
	uint64_t* arr = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* idx = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = ((uint64_t*)(arr[0]))[idx[0]];
}

void set64fn() {
	FnRecord* self = find_fn(symbols, L"set64");
	uint64_t* arr = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* idx = (uint64_t*)(data_buff.data + self->args[1]->loc);
	uint64_t* val = (uint64_t*)(data_buff.data + self->args[2]->loc);

	((uint64_t*)(arr[0]))[idx[0]] = val[0];
}

void getfn() {
	register uint64_t ret asm("rax");

	FnRecord* self = find_fn(symbols, L"get");
	uint64_t* arr = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* idx = (uint64_t*)(data_buff.data + self->args[1]->loc);

	ret = ((uint8_t*)(arr[0]))[idx[0]];
}

void setfn() {
	FnRecord* self = find_fn(symbols, L"set");
	uint64_t* arr = (uint64_t*)(data_buff.data + self->args[0]->loc);
	uint64_t* idx = (uint64_t*)(data_buff.data + self->args[1]->loc);
	uint64_t* val = (uint64_t*)(data_buff.data + self->args[2]->loc);

	((uint8_t*)(arr[0]))[idx[0]] = val[0];
}

void freefn() {
	FnRecord* self = find_fn(symbols, L"free");
	uint64_t* arr = (uint64_t*)(data_buff.data + self->args[0]->loc);
	free((uint8_t*)(arr[0]));
}

void loadfn() {
	//register uint64_t what asm("rax");
	FnRecord* self = find_fn(symbols, L"load");
	uint8_t* what = data_buff.data + self->args[0]->loc;
	uint64_t lgt = ((uint64_t*)what)[0];
	const wchar_t* str = (wchar_t*)what + sizeof(uint64_t) / sizeof(wchar_t);

	mbstate_t state;
	ssize_t s_lgt = wcsrtombs(NULL, &str, 0, &state) + 1;
	char* dst = malloc(sizeof(char) * s_lgt);
	wcsrtombs(dst, &str, s_lgt, &state);

	FILE* old_file = curr_file;
	curr_file = fopen(dst, "r"); 

	uint8_t quit = 0;
	compile(&quit);

	if(!quit && last) {
		uint64_t ret = execute();
		print(ret);
	}

	curr_file = old_file;
}

void update(uint8_t force) {
	if(last != NULL || force) {
		if(mprotect(code, code_end - code, PROT_READ | PROT_WRITE) == -1) {
			ERROR(L"mprotect to PROT_RW failed\n");
		}
		if(codeptr + (code_buff.lgt - (ssize_t)last) >= code_end) {
			ssize_t a = (code_end - code) + (code_buff.lgt - (ssize_t)last);
			ssize_t b = (code_end - code) * 2;
			void* new_code = mmap(NULL, a > b? a:b, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
			memcpy(new_code, code, code_end - code);
			munmap(code, code_end - code);
			ssize_t delta = codeptr - code;
			ssize_t delta_lgt = code_end - code;
			code = new_code;
			code_end = code + delta_lgt;
			codeptr = code + delta;

			reset_reloc_ptrs();
		}

		relocate();
		memcpy(codeptr, code_buff.data + (ssize_t)last, code_buff.lgt - (ssize_t)last);

#ifdef DEBUG
		wprintf(L"Post-relocation code:\n");
		for(uint8_t* i = codeptr; i < codeptr + (code_buff.lgt - (ssize_t)last); i++) {
			wprintf(L"%02X ", *i);
			if(i - codeptr != 0 && (i - codeptr) % 8 == 0){
				wprintf(L"\n");
			}
		}
		wprintf(L"\n");
#endif
	}
}

uint64_t execute() {
	update(0);
	if(mprotect(code, code_end - code, PROT_EXEC) == -1) {
		ERROR(L"mprotect to PROT_EXEC failed\n");
	}

	void*(*fn)() = (void*(*)())codeptr;
	uint64_t ret = (uint64_t)fn();

	codeptr += (code_buff.lgt - (ssize_t)last);

	return ret;
}

void init_compiler() {
	symbols = calloc(sizeof(SymCell), 1);

	register_fn(L"print", (void*(*)())printfn, 1, (wchar_t*[1]){L"what"});
	register_fn(L"printhex", (void*(*)())printhexfn, 1, (wchar_t*[1]){L"what"});
	register_fn(L"load", (void*(*)())loadfn, 1, (wchar_t*[1]){L"what"});

	register_fn(L"%", (void*(*)())modfn, 2, (wchar_t*[2]){L"lhs", L"rhs"});
	register_fn(L"/", (void*(*)())divfn, 2, (wchar_t*[2]){L"lhs", L"rhs"});
	register_fn(L"*", (void*(*)())mulfn, 2, (wchar_t*[2]){L"lhs", L"rhs"});
	register_fn(L"-", (void*(*)())subfn, 2, (wchar_t*[2]){L"lhs", L"rhs"});
	register_fn(L"+", (void*(*)())addfn, 2, (wchar_t*[2]){L"lhs", L"rhs"});

	register_fn(L"reserve", (void*(*)())reservefn, 1, (wchar_t*[1]){L"what"});
	register_fn(L"reserve64", (void*(*)())reserve64fn, 1, (wchar_t*[1]){L"what"});
	register_fn(L"free", (void*(*)())freefn, 1, (wchar_t*[1]){L"what"});
	register_fn(L"get", (void*(*)())getfn, 2, (wchar_t*[2]){L"arr", L"idx"});
	register_fn(L"get64", (void*(*)())get64fn, 2, (wchar_t*[2]){L"arr", L"idx"});
	register_fn(L"set", (void*(*)())setfn, 3, (wchar_t*[3]){L"arr", L"idx", L"val"});
	register_fn(L"set64", (void*(*)())set64fn, 3, (wchar_t*[3]){L"arr", L"idx", L"val"});

	//update(1);
}

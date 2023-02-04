#pragma once

#include "util.h"
#include "string.h"
#include "ast.h"
#include "parser.h"

#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <wchar.h>

void init_compiler();

void codebyte(uint8_t byte);
void databyte(uint8_t byte);
void compile(uint8_t* eof);
void relocate();

void reset_reloc_ptrs();

typedef enum {
	Reloc8 = 0,
	Reloc16,
	Reloc32,
	Reloc64
} RelocSize;

void code_reloc(uint64_t src, uint64_t base, uint8_t rdata, RelocSize size, uint8_t relative);
void data_reloc(uint64_t src, uint64_t base, uint8_t rdata, RelocSize size, uint8_t relative);

typedef struct Reloc {
	uint64_t src;
	uint64_t base;
	uint8_t rdata; // 1 = relative to data; 0 = relative to code.
	RelocSize size;
	uint8_t relative; // 1 = relative to insert location; 0 = relative only to base/origin.
} Reloc;

#define BUMP_TYPE Reloc
#define BUMP_NAME RelocBuff
#include "bump_interface.h"

struct SymCell;

typedef enum {
	loc_reg,
	loc_mem,
	loc_stack,
	loc_discard, // 'effect' in the ddcg paper
} loc_t;

typedef enum {
	lab_none,
	lab_cond,
	lab_loop,
} lab_t;

typedef struct LocRecord {
	loc_t type;
	uint64_t loc;
	lab_t lab_type;
	lab_t lab0;
	lab_t lab1;
} LocRecord;

#define BUMP_TYPE LocRecord
#define BUMP_NAME LocBuff
#include "bump_interface.h"

typedef struct {
	wchar_t* name;
	loc_t loc_type;
	uint64_t loc;
} Symbol;

struct SymCell;

typedef struct {
	uint64_t loc;
	uint8_t n_args;
	Symbol** args;
	struct SymCell* env;
} FnRecord;

#define BUMP_TYPE Symbol
#define BUMP_NAME SymBuff
#include "bump_interface.h"

#define BUMP_TYPE FnRecord
#define BUMP_NAME FnBuff
#include "bump_interface.h"

typedef struct SymCell {
	SymBuff buff;
	struct SymCell* prev;
} SymCell;

Symbol* find_symb(SymCell* cell, wchar_t* name);
FnRecord* find_fn(SymCell* cell, wchar_t* name);
void bind_args(SymCell* env, wchar_t* fn_name, AST* args);

extern ByteBuff code_buff;
extern ByteBuff data_buff;
extern RelocBuff code_reloc_buff;
extern uint64_t code_reloc_ptr;
extern RelocBuff data_reloc_buff;
extern uint64_t data_reloc_ptr;
extern void* last;

extern uint8_t* code;
extern uint8_t* codeptr;
extern uint8_t* code_end;

extern SymCell* symbols;
extern FnBuff functions;

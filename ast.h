#pragma once

#include <stdlib.h>

typedef enum ASTType {
	ast_none = 0,

	ast_cond,
	ast_loop,
	ast_break,

	ast_def,

	ast_num,
	ast_str,
	ast_var,
	ast_fn,

	ast_call,

	ast_n_types
} ASTType;

typedef struct AST {
	ASTType type;
	void* data;
	struct AST* child0;
	struct AST* child1;
	struct AST* next;
	ssize_t ch;
	ssize_t lin;
} AST;

#define BUMP_TYPE AST
#define BUMP_NAME ASTBuff
#include "bump_interface.h"

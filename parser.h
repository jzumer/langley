#pragma once

#include "string.h"
#include "ast.h"
#include "token.h"
#include "rule.h"

#include <wchar.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <wctype.h>

AST* try_rule(wchar_t* rule);

AST* name();
AST* number();
AST* def();
AST* call();
AST* fn();
AST* expr();
AST* stmt();
AST* nothing();
AST* program();

void register_rules();

void pop_tokens(ssize_t n);
void push_token(wchar_t* str, ssize_t lgt, ssize_t ch, ssize_t lin);
ssize_t readtok(FILE* fd, wchar_t* buff, ssize_t max);
Token* next_token();

extern TokenBuff tokens;
extern FILE* curr_file;
extern wchar_t* curr_fname;
extern wchar_t* fname_repl;
extern ssize_t curr_char;
extern ssize_t curr_line;

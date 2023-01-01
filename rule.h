#pragma once

#include "string.h"
#include "util.h"
#include "ast.h"

#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>

typedef struct Rule {
	wchar_t* name;
	AST*(*parse)();
} Rule;

#define BUMP_TYPE Rule
#define BUMP_NAME RuleBuff
#include "bump_interface.h"

ssize_t rule_index(RuleBuff* rules, wchar_t* rule);
AST* do_rule(RuleBuff* rules, wchar_t* rule);
void add_rule(RuleBuff* rules, wchar_t* rule, AST*(*fn)());

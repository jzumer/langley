#pragma once

#include "rule.h"
#include "ast.h"
#include "token.h"
#include "compiler.h"

#define BUMP_TYPE Rule
#define BUMP_NAME RuleBuff
#include "bump.h"

#define BUMP_START_CAP 8
#define BUMP_TYPE Token
#define BUMP_NAME TokenBuff
#include "bump.h"

#define BUMP_START_CAP 256
#define BUMP_TYPE wchar_t
#define BUMP_NAME CharBuff
#include "bump.h"

#define BUMP_TYPE AST
#define BUMP_NAME ASTBuff
#include "bump.h"

#define BUMP_START_CAP 256
#define BUMP_TYPE uint8_t
#define BUMP_NAME ByteBuff
#include "bump.h"

#define BUMP_TYPE Reloc
#define BUMP_NAME RelocBuff
#include "bump.h"

#define BUMP_TYPE Symbol
#define BUMP_NAME SymBuff
#include "bump.h"

#define BUMP_TYPE FnRecord
#define BUMP_NAME FnBuff
#include "bump.h"

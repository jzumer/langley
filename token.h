#pragma once

#include "string.h"
#include "util.h"

#include <ctype.h>
#include <stdint.h>

typedef struct Token {
	wchar_t* str;
	ssize_t size;
	ssize_t ch;
	ssize_t lin;
} Token;

#define BUMP_START_CAP 8
#define BUMP_TYPE Token
#define BUMP_NAME TokenBuff
#include "bump_interface.h"

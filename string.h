#pragma once

#include <string.h>
#include <wchar.h>
#include <ctype.h>
#include <stdint.h>

#define BUMP_START_CAP 256
#define BUMP_TYPE wchar_t
#define BUMP_NAME CharBuff
#include "bump_interface.h"

#define BUMP_START_CAP 256
#define BUMP_TYPE uint8_t
#define BUMP_NAME ByteBuff
#include "bump_interface.h"

#pragma once

#include <stdio.h>

#define ERROR(fmt, ...) do { \
    fwprintf(stderr, fmt, ##__VA_ARGS__); \
    exit(1); \
} while (0)

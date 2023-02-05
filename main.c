#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <wchar.h>

#include <unistd.h>

#include <sys/mman.h>

#include "util.h"
#include "compiler.h"
#include "parser.h"
#include "types.h"

uint64_t ret = 0;

void print() {
	wprintf(L"%d; OK\n", ret);
}

uint8_t quit = 0;

int main() {
	ByteBuff_push(&code_buff, 1); // hack to start 'last' at 1 if we do compile

	register_rules();
	init_compiler();

	code = mmap(NULL, 1024, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
	codeptr = code;
	code_end = code + 1024;

	fwide(stdin, 1);
	while(!quit) {
		compile(&quit);
#ifdef DEBUG
		wprintf(L"Codeptr: %p; (delta: %d). Code:\n", codeptr, codeptr - code);
		for(ssize_t i = 0; i < code_buff.lgt; i++) {
			wprintf(L"%02X ", code_buff.data[i]);
			if(i != 0 && i % 8 == 0) { wprintf(L"\n"); }
		}
		wprintf(L"\n");
#endif
		if(!quit && last) {
			execute();
			print();
		}
	}

//	for(int i = 0; i < tokens.lgt; i++) {
//		wprintf(L"%ls ", tokens.data[i].str);
//	}
//	wprintf(L"\n");

	return 0;
}

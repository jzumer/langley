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

void execute() {
	if(last != NULL) {
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
			wprintf(L"%x ", *i);
			if(i - codeptr != 0 && (i - codeptr) % 8 == 0){
				wprintf(L"\n");
			}
		}
		wprintf(L"\n");
#endif

		if(mprotect(code, code_end - code, PROT_EXEC) == -1) {
			ERROR(L"mprotect to PROT_EXEC failed\n");
		}

		void*(*fn)() = (void*(*)())codeptr;
		ret = (uint64_t)fn();

		codeptr += (code_buff.lgt - (ssize_t)last);
	}
}

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
			wprintf(L"%x ", code_buff.data[i]);
			if(i != 0 && i % 8 == 0) { wprintf(L"\n"); }
		}
		wprintf(L"\n");
#endif
		if(last) {
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

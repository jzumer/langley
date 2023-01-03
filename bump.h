#ifndef BUMP_TYPE
#error BUMP_TYPE must be defined
#endif
#ifndef BUMP_NAME
#error BUMP_NAME must be defined
#endif

#ifndef BUMP_START_CAP
#define BUMP_START_CAP 8
#endif
#define INIT_POST _init
#define ALLOC_POST _alloc
#define GET_POST _get
#define PUSH_POST _push
#define POP_POST _pop

#define CONCATX(A, B) A ## B
#define CONCAT(A, B) CONCATX(A, B)

void CONCAT(BUMP_NAME, INIT_POST)(BUMP_NAME * buff, uint32_t start_cap) {
	buff->cap = start_cap * sizeof(BUMP_TYPE) ;
	buff->data = calloc(sizeof(BUMP_TYPE), buff->cap);
	buff->lgt = 0;
}

BUMP_TYPE * CONCAT(BUMP_NAME, PUSH_POST) (BUMP_NAME * buff, uint32_t n) {
	uint64_t two_n = n * 2;
	if(buff->cap == 0) {
		CONCAT(BUMP_NAME, INIT_POST)(buff, BUMP_START_CAP > two_n? BUMP_START_CAP : two_n);
	} else {
		if(buff->lgt + n >= buff->cap) {
			uint64_t prev_cap = buff->cap;
			while(buff->lgt + n >= buff->cap) {
				buff->cap *= 2;
			}
			buff->data = realloc(buff->data, buff->cap * sizeof(BUMP_TYPE));
		}
	}

	buff->lgt += n;
	return &(buff->data[buff->lgt-n]);
}

void CONCAT(BUMP_NAME, POP_POST) (BUMP_NAME * buff, uint32_t n) {
	if(buff->lgt <= n) { buff->lgt = 0; }
	else { buff->lgt -= n; }
}

BUMP_TYPE * CONCAT(BUMP_NAME, ALLOC_POST) (BUMP_NAME * buff) {
	return CONCAT(BUMP_NAME, PUSH_POST)(buff, 1);
}

//BUMP_TYPE * CONCAT(BUMP_NAME, GET_POST) (BUMP_NAME * buff, uint64_t idx) {
//	if(idx < buff->lgt) {
//		return &(buff->data[idx]);
//	}
//
//	return NULL;
//}

#undef CONCATX
#undef CONCAT

#undef GET_POST
#undef ALLOC_POST
#undef PUSH_POST
#undef POP_POST
#undef INIT_POST
#undef BUMP_NAME
#undef BUMP_TYPE
#undef BUMP_START_CAP

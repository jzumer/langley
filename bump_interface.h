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

typedef struct BUMP_NAME {
	BUMP_TYPE * data;
	int64_t cap;
	int64_t lgt;
} BUMP_NAME ;

void CONCAT(BUMP_NAME, INIT_POST)(BUMP_NAME * buff, uint32_t start_cap) ;
BUMP_TYPE * CONCAT(BUMP_NAME, PUSH_POST) (BUMP_NAME * buff, uint32_t n) ;
void CONCAT(BUMP_NAME, POP_POST) (BUMP_NAME * buff, uint32_t n) ;
BUMP_TYPE * CONCAT(BUMP_NAME, ALLOC_POST) (BUMP_NAME * buff) ;

#undef CONCATX
#undef CONCAT
#undef GET_POST
#undef ALLOC_POST
#undef PUSH_POST
#undef POP_POST
#undef INIT_POST
#undef BUMP_START_CAP
#undef BUMP_NAME
#undef BUMP_TYPE

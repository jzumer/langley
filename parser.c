#include "parser.h"

ssize_t curr_line = 1;
ssize_t curr_char = 1;

FILE* curr_file;
wchar_t* curr_fname;
wchar_t* fname_repl = L"<input>";

CharBuff program_strings = {};
ASTBuff ast_buff = {};

RuleBuff rules = {};
CharBuff rule_names = {};

TokenBuff tokens = {};
CharBuff token_names = {};
ssize_t current_token = -1;

void pop_tokens(ssize_t n) {
	for(; n > 0; n--) {
		ssize_t lgt = tokens.data[tokens.lgt-1].size;
		CharBuff_pop(&token_names, lgt);
		TokenBuff_pop(&tokens, 1);
	}
}

ssize_t readtok(FILE* fd, wchar_t* buff, ssize_t max) {
	ssize_t idx = 0;
	uint8_t reading = 0;
	uint8_t the_str = 0;
	uint8_t str_esc = 0;

	while(1) {
		wchar_t next = fgetwc(fd);
		if(next == WEOF || feof(fd)) { break; }

		if(next == '\n') { curr_line++; curr_char = 1; }
		else { curr_char++; }

		if(iswspace(next)) {
			if(!the_str && reading) { break; }
		} else {
			if(!reading) { reading = 1; }
		}

		if(reading) {
			if(the_str && !str_esc && (next == '\\')) {
				str_esc = 1;
			} else if(the_str && str_esc) {
				switch(next) {
					case '\\':
						buff[idx] = '\\';
						break;
					case 'n':
						buff[idx] = '\n';
						break;
					case 't':
						buff[idx] = '\t';
						break;
					default:
						ERROR(L"Invalid escape code: '\\%c'", next);
				}
				idx++;
			} else if(the_str && (next == '"')) {
				if(!str_esc) {
					the_str = 0;
					buff[idx] = '"';
					idx++;
				} else {
					buff[idx] = next;
					idx++;
				}
			} else if(!the_str && (next == '"')) {
				the_str = 1;
				buff[idx] = '"';
				idx++;
			} else {
				buff[idx] = next;
				idx++;
			}

			if(the_str && !str_esc && (next != '\\')) {
				str_esc = 0;
			}

			if(idx > max) { break; }
		}
	}

	return idx;
}

Token* next_token() {
	Token* ret = NULL;

	if(current_token+1 < tokens.lgt) {
		current_token++;
		ret = &tokens.data[current_token];
	} else {
		CharBuff buffer = {};
		buffer.cap = 0;

		ssize_t batch_size = 256;
		wchar_t tmp_buff[256];

		ssize_t tok_line = -1;
		ssize_t tok_char = -1;

		uint8_t new = 1;
		while(1) {
			memset(tmp_buff, 0, batch_size);
			ssize_t n_read = readtok(curr_file, tmp_buff, batch_size-1);

			if(n_read == 0) {
				break;
			} else {
				if(new) {
					new = 0;
					tok_line = curr_line;
					tok_char = curr_char - n_read;
				}
				wchar_t* data = CharBuff_push(&buffer, n_read);
				wmemcpy(data, tmp_buff, n_read);
				if(n_read <= batch_size-1) {
					break;
				}
			}
		}

		if(buffer.lgt > 0) {
			push_token(buffer.data, buffer.lgt, tok_line, tok_char);
			current_token++;
			ret = &tokens.data[current_token];
		}
	}

	return ret;
}

Token* peek_token() {
	Token* ret = next_token();
	current_token--;
	return ret;
}

void push_token(wchar_t* str, ssize_t lgt, ssize_t ch, ssize_t lin) {
	Token* tok = TokenBuff_alloc(&tokens);
	tok->str = CharBuff_push(&token_names, lgt + 1);
	wmemcpy(tok->str, str, lgt);
	tok->str[lgt] = '\0';
	tok->size = lgt;
	tok->ch = ch;
	tok->lin = lin;
}

AST* try_rule(wchar_t* rule) {
	ssize_t this_token = current_token;
	ssize_t ast_lgt = ast_buff.lgt;

	AST* ret = do_rule(&rules, rule);
	if(!ret) {
		current_token = this_token;
		ASTBuff_pop(&ast_buff, ast_buff.lgt - ast_lgt);
	}
	return ret;
}

AST* str() {
	AST* ret = ASTBuff_push(&ast_buff, 1);

	uint64_t val = 0;
	Token* tok = next_token();
	if(!tok) { return NULL; }
	if(tok->size < 2 || tok->str[0] != '"' || tok->str[tok->size-1] != '"') { return NULL; }

	ret->type = ast_str;
	wchar_t* s = calloc((tok->size) * sizeof(wchar_t) + sizeof(uint64_t), 1);
	((uint64_t*)s)[0] = tok->size - 3;
	wcscpy(s+(sizeof(uint64_t) / sizeof(wchar_t)), tok->str+1);
	s[tok->size + (sizeof(uint64_t) / sizeof(wchar_t)) - 2] = '\0';
	ret->data = (void*)s;
	ret->lin = tok->lin;
	ret->ch = tok->ch;

	return ret;
}

AST* cond() {
	AST* ret = ASTBuff_push(&ast_buff, 1);

	Token* tok = next_token();
	if(!tok) { return NULL; }

	if(wcscmp(tok->str, L"if") != 0) { return NULL; }

	ret->type = ast_cond;
	ret->data = tok->str;
	ret->lin = tok->lin;
	ret->ch = tok->ch;

	AST* c = try_rule(L"infix-expr");
	if(!c) { return NULL; }

	ret->child0 = c;

	tok = next_token();
	if(!tok) { return NULL; }

	if(wcscmp(tok->str, L"then") != 0) { return NULL; }

	AST* iftrue = try_rule(L"infix-expr");
	if(!iftrue) { return NULL; }
	
	tok = next_token();
	if(!tok) { return NULL; }

	ret->child1 = iftrue;

	if(wcscmp(tok->str, L";") != 0) {
		if(wcscmp(tok->str, L"else") == 0) {
			AST* iffalse = try_rule(L"infix-expr");
			if(!iffalse) { return NULL; }

			tok = next_token();
			if(!tok) { return NULL; }

			if(wcscmp(tok->str, L";") != 0) { return NULL; }

			ret->child1->next = iffalse;

			return ret;
		} else {
			return NULL;
		}
	} else {
		return ret;
	}
}

AST* loop() {
	AST* ret = ASTBuff_push(&ast_buff, 1);

	Token* tok = next_token();
	if(!tok) { return NULL; }

	if(wcscmp(tok->str, L"loop") != 0) { return NULL; }

	ret->type = ast_loop;
	ret->data = tok->str;
	ret->lin = tok->lin;
	ret->ch = tok->ch;

	AST* e = try_rule(L"infix-expr");
	if(!e) { return NULL; }
	ret->child0 = e;

	return ret;
}

AST* brek() {
	AST* ret = ASTBuff_push(&ast_buff, 1);
	
	Token* tok = next_token();
	if(!tok) { return NULL; }

	if(wcscmp(tok->str, L"break") != 0) { return NULL; }

	ret->type = ast_break;
	ret->data = tok->str;
	ret->lin = tok->lin;
	ret->ch = tok->ch;

	return ret;
}

AST* name() {
	AST* ret = ASTBuff_push(&ast_buff, 1);

	Token* tok = next_token();
	if(!tok) { return NULL; }
	if(tok->size == 1) {
		switch(tok->str[0]) {
			case L'(':
			case L')':
			case L'+':
			case L'-':
			case L'*':
			case L'/':
			case L'%':
				return NULL;
			default:
				break;
		}
	}
	ret->type = ast_var;
	ret->data = tok->str;
	ret->lin = tok->lin;
	ret->ch = tok->ch;

	return ret;
}

AST* number() {
	AST* ret = ASTBuff_push(&ast_buff, 1);

	uint8_t num = 1;
	uint64_t val = 0;
	Token* tok = next_token();
	if(!tok) { return NULL; }
	for(ssize_t i = 0; i < tok->size; i++) {
		num &= iswdigit(tok->str[i]);
		if(!num) { break; }
		val *= 10;
		val += tok->str[i] - '0';
	}

	if(num) {
		ret->type = ast_num;
		ret->data = (void*)val;
		ret->lin = tok->lin;
		ret->ch = tok->ch;
	} else {
		ret = NULL;
	}

	return ret;
}

AST* def() {
	AST* var = try_rule(L"name");
	if(!var) { return 0; }
	Token* eq = next_token();
	if(!eq || wcscmp(eq->str, L":=") != 0) { return 0; }
	AST* val = try_rule(L"infix-expr");
	if(!val) { return 0; }

	AST* ret = ASTBuff_push(&ast_buff, 1);
	ret->type = ast_def;
	ret->child0 = var;
	ret->child1 = val;

	return ret;
}

AST* call() {
	// fn ( x , y ... )
	AST* f = try_rule(L"name");
	if(!f) { return 0; }

	AST* ret = ASTBuff_push(&ast_buff, 1);
	ret->type = ast_call;
	ret->child0 = f;
	AST* next_arg = NULL;

	Token* paren = next_token();
	if(!paren || wcscmp(paren->str, L"(") != 0) { return NULL; }
	Token* latest = paren;

	while(latest) {
		AST* ex = try_rule(L"infix-expr");
		if(!ex) { break; }
		if(!next_arg) {
			ret->child1 = next_arg = ex;
		} else {
			next_arg->next = ex;
			next_arg = next_arg->next;
		}
		latest = next_token();
		if(!latest || wcscmp(latest->str, L",") != 0) { break; }
	}

	if(!latest) { return NULL; }
	if(latest && wcscmp(latest->str, L"(") == 0) { latest = next_token(); }
	if(!latest || wcscmp(latest->str, L")") != 0) { return NULL; }

	return ret;
}

AST* infix() {
	AST* lhs = try_rule(L"expr");
	if(!lhs) { return NULL; }
	Token* maybe_op = next_token();
	if(!maybe_op || maybe_op->size != 1) { return NULL; }
	switch(maybe_op->str[0]) {
		case L'*':
		case L'/':
		case L'-':
		case L'+':
		case L'%':
		case L'=':
		case L'<':
		case L'>':
			break;
		default:
			return NULL;
	}
	AST* rhs = try_rule(L"infix-expr");
	if(!rhs) { return NULL; }

	AST* ret = ASTBuff_push(&ast_buff, 1);
	AST* fn_node = ASTBuff_push(&ast_buff, 1);
	ret->type = ast_call;
	ret->child0 = fn_node;
	AST* next_arg = NULL;

	fn_node->type = ast_var;
	fn_node->data = calloc(sizeof(wchar_t), 2);
	((wchar_t*)fn_node->data)[0] = maybe_op->str[0];

	ret->child1 = lhs;
	lhs->next = rhs;

	return ret;
}

AST* fn() {
	// | x , y ... | expr
	Token* bar = next_token();
	if(!bar || wcscmp(bar->str, L"|") != 0) { return 0; }
	Token* latest = bar;

	AST* ret = ASTBuff_push(&ast_buff, 1);
	ret->type = ast_fn;
	AST* next_arg = NULL;

	do {
		AST* name = try_rule(L"name");
		if(!name) { return 0; }
		if(!next_arg) {
			ret->child0 = next_arg = name;
		} else {
			next_arg->next = name;
			next_arg = next_arg->next;
		}
		latest = next_token();
		if(!latest || wcscmp(latest->str, L",") != 0) { break; }
	} while (latest && wcscmp(latest->str, L"|") != 0);

	if(!latest) { return 0; }
	if(wcscmp(latest->str, L"|") != 0) { return 0; }

	AST* ex = try_rule(L"infix-expr");
	if(!ex) { return 0; }
	ret->child1 = ex;

	return ret;
}

AST* infix_expr() {
	uint8_t n_subrules = 2;
	wchar_t* subrules[2] = {L"infix", L"expr"};
	for(uint8_t i = 0; i < n_subrules; i++) {
		AST* ret = try_rule(subrules[i]);
		if(ret) {
			return ret;
		}
	}

	return NULL;
}

AST* expr() {
	Token* tok = peek_token();
	AST* ret = NULL;
	AST* next = NULL;
	if(wcscmp(tok->str, L"{") == 0) {
		next_token();
		while(1) {
			AST* stmt = try_rule(L"stmt");
			if(!stmt) {
				tok = next_token();
				if(wcscmp(tok->str, L"}") != 0) {
					return NULL;
				}
				return ret;
			}
			if(!next) {
				ret = next = stmt;
			} else {
				next->next = stmt;
				next = next->next;
			}
			tok = peek_token();
			if(wcscmp(tok->str, L"}") == 0) {
				next_token();
				return ret;
			}
		}
	}

	uint8_t n_subrules = 8;
	wchar_t* subrules[8] = {L"str", L"fn", L"cond", L"loop", L"break", L"call", L"number", L"name"};
	for(uint8_t i = 0; i < n_subrules; i++) {
		ret = try_rule(subrules[i]);
		if(ret) {
			return ret;
		}
	}

	return NULL;
}

AST* nothing() {
	Token* tok = next_token();
	if(tok) { return NULL; }
	else {
		AST* ret = ASTBuff_push(&ast_buff, 1);
		ret->type = ast_none;
		return ret;
	}
}

AST* stmt() {
	AST* ret = try_rule(L"def");
	if(ret) { goto check_sc; }
	ret = try_rule(L"infix-expr");
	if(ret) { goto check_sc; }
	return NULL;

check_sc: {
		Token* tok = peek_token();
		if(tok && wcscmp(tok->str, L";") == 0) {
			next_token();
			return ret;
		} else {
			return NULL;
		}
	}
}

AST* program() {
	AST* ret = try_rule(L"nothing");
	if(ret) { return ret; }
	return try_rule(L"stmt");
}

void register_rules() {
	add_rule(&rules, L"nothing", nothing);
	add_rule(&rules, L"number", number);
	add_rule(&rules, L"str", str);
	add_rule(&rules, L"infix", infix);
	add_rule(&rules, L"name", name);
	add_rule(&rules, L"def", def);
	add_rule(&rules, L"call", call);
	add_rule(&rules, L"fn", fn);
	add_rule(&rules, L"break", brek);
	add_rule(&rules, L"loop", loop);
	add_rule(&rules, L"cond", cond);
	add_rule(&rules, L"stmt", stmt);
	add_rule(&rules, L"expr", expr);
	add_rule(&rules, L"infix-expr", infix_expr);
	add_rule(&rules, L"program", program);
}

#include "rule.h"

ssize_t rule_index(RuleBuff* rules, wchar_t* rule) {
	for(uint32_t i = 0; i < rules->lgt; i++) {
		if(wcscmp(rules->data[i].name, rule) == 0) {
			return i;
		}
	}
	return -1;
}

AST* do_rule(RuleBuff* rules, wchar_t* rule) {
	ssize_t idx = rule_index(rules, rule);
	if(idx < 0) {
		ERROR(L"Could not find rule '%s'\n", rule);
	} else {
		AST* recognized = rules->data[idx].parse();
		return recognized;
	}
}

void add_rule(RuleBuff* rules, wchar_t* rule, AST*(*fn)()) {
	Rule* the_rule = RuleBuff_push(rules, 1);
	the_rule->name = rule;
	the_rule->parse = fn;
}

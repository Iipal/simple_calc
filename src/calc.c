#define _GNU_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <err.h>
#include <ctype.h>
#include <assert.h>

#define MSG_USAGE "Usage: ./calc \"<expr-1>\" ... \"<expr-N>\""
#define MSG_VALID "1234567890+-*/()"

typedef long long expr_t;

typedef enum e_expr_opeartion {
	e_op_invalid,
	e_op_add,
	e_op_sub,
	e_op_mul,
	e_op_div
} expr_op;

struct s_expr_data {
	expr_t	l_value;
	expr_t	r_value;
	expr_op	op;
} __attribute__((aligned(__BIGGEST_ALIGNMENT__)));

static void __attribute__((noreturn))	print_help(void);

static char	*trim_whitespaces(const char *src);
static void	syntax_validation_throw(const char *expr);

static expr_t	expr_parser(char *expr);

int	main(int argc, char *argv[]) {
	--argc; ++argv;
	if (argc && (!strcmp(*argv, "--help") || !strcmp(*argv, "-h")))
		print_help();
	if (!argc)
		errx(EXIT_FAILURE, MSG_USAGE);

	char	*trimmed_expr = NULL;
	while (argc) {
		assert(trimmed_expr = trim_whitespaces(*argv));
		syntax_validation_throw(trimmed_expr);
		printf("%lld\n", expr_parser(trimmed_expr));
		free(trimmed_expr);
		--argc; ++argv;
	}
}

static inline void __attribute__((noreturn))	print_help(void) {
	printf(MSG_USAGE "\n"
	 "Available symbols: " MSG_VALID "\n"
	 "Example: ./calc \"(2 + 4) * 5\" \"(2 + 2)\" \"(2 * 4 + 5) - 3\"\n"
	 "Output:\n30\n4\n10\n");
	_Exit(EXIT_SUCCESS);
}

static inline char	*trim_whitespaces(const char *src) {
	const size_t	src_len = strlen(src);
	size_t			trim_len = 0;
	char			*out = NULL;

	for (size_t i = 0; src_len > i; i++)
		if (!isspace(src[i]))
			++trim_len;
	if (!(out = calloc(trim_len + 1, sizeof(char))))
		return NULL;
	for (size_t j = 0, i = 0; src_len > i; i++)
		if (!isspace(src[i]))
			out[j++] = src[i];
	return out;
}

static inline void	syntax_validation_throw(const char *expr) {
# define _is_valid_expr_sym(c) (isdigit((int)(c)) \
	|| '+' == (c) || '-' == (c) || '/' == (c) || '*' == (c) \
	|| '(' == (c) || ')' == (c))

	const char *iptr = expr;
	for (; iptr && *iptr && _is_valid_expr_sym(*iptr); iptr++)
			;
	if (*iptr)
		errx(EXIT_FAILURE, "invalid symbol -- %c [ '%s' : '%s' ]\n"
			"valid symbols are: \'" MSG_VALID "\'\n", *iptr, expr, iptr);

# undef _is_invalid_expr_sym
}

# define _skip_digits(_s) while ((_s) && *(_s) && isdigit(*(_s))) ++(_s);

# define _is_sym_op_priority(c) ('*' == (c) || '/' == (c))
# define _is_sym_op_default(c)  ('+' == (c) || '-' == (c))
# define _is_sym_op_any(c)      ('+' == (c) || '-' == (c) \
						      || '*' == (c) || '/' == (c))

static expr_t	parse_op_priority(char *expr);
static expr_t	parse_op_default(const char *expr, struct s_expr_data *rec_e);

static expr_t	expr_run(const struct s_expr_data *restrict ed);

static inline expr_op	get_expr_op(const char op_sym) {
	static const char	valids[] = { '+', '-', '*', '/', 0 };
	size_t				selector;

	selector = 0;
	while (valids[selector] && valids[selector] != op_sym)
		++selector;
	if (!valids[selector])
		return e_op_invalid;
	return (expr_op)(selector + 1);
}

static inline expr_t	choose_op_parser(char *expr) {
	char *iptr = expr;
	while (iptr && *iptr && !_is_sym_op_priority(*iptr))
		++iptr;
	return (!!*iptr ? parse_op_priority(expr) : parse_op_default(expr, NULL));
}

static inline bool	check_signed_value(const char *expr) {
	return (('-' == *expr)
		? (_is_sym_op_any(expr[-1]) || !expr[-1])
		: ('-' == expr[-1] && _is_sym_op_any(expr[-2])));
}


static expr_t	parse_op_default(const char *expr, struct s_expr_data *rec_e) {
	struct s_expr_data	ed = { 0LL, 0LL, e_op_invalid };
	const bool	is_signed_lvalue = check_signed_value(expr);
	const char	*iptr = expr + is_signed_lvalue;

	if (!rec_e) {
		if (!_is_sym_op_any(*iptr)) {
			_skip_digits(iptr);
			ed.op = get_expr_op(*iptr);
		}
	} else {
		ed.op = rec_e->op;
	}
	if (!rec_e && !iptr[1])
		ed.op = e_op_invalid;
	ed.l_value = rec_e ? rec_e->l_value : atoll(expr);
	ed.r_value = atoll(iptr++ + !rec_e - is_signed_lvalue);
	if (0 > ed.r_value)
		++iptr;
	_skip_digits(iptr);
	if (*iptr) {
		ed.l_value = expr_run(&ed);
		ed.op = get_expr_op(*iptr);
		return parse_op_default(++iptr, &ed);
	}
	return expr_run(&ed);
}

static expr_t	parse_op_priority(char *expr) {
	struct s_expr_data	ed = { 0LL, 0LL, e_op_invalid };
	char	*iptr = expr;
	char	*l_operand = NULL;

	for (; iptr && *iptr && !_is_sym_op_priority(*iptr); iptr++)
		if (isdigit(*iptr) && ((_is_sym_op_default(iptr[-1]))
			|| ('-' == iptr[-1] && _is_sym_op_default(iptr[-2])) || !iptr[-1]))
			l_operand = iptr;
	if (!*iptr || !iptr[1] || !l_operand)
		errx(EXIT_FAILURE, "Invalid expression.");
	l_operand -= check_signed_value(l_operand);
	ed.op = get_expr_op(*iptr);
	ed.l_value = atoll(l_operand);
	ed.r_value = atoll(++iptr);
	if (0 > ed.r_value)
		++iptr;
	_skip_digits(iptr);
	if (*iptr || l_operand != expr) {
		char	*end_dup = strdup(iptr);
		char	*res_str = calloc(25, sizeof(char));
		size_t	res_len = 0;

		assert(end_dup);
		sprintf(res_str, "%lld", expr_run(&ed));
		res_len = strlen(res_str);
		memcpy(expr + (l_operand - expr), res_str, res_len);
		strcpy(expr + (l_operand - expr) + res_len, end_dup);
		free(res_str);
		free(end_dup);
		return choose_op_parser(expr);
	}
	return expr_run(&ed);
}

static char	*find_pth_close(char *start) {
	size_t	nested_depth = 0;

	for (char *iptr = start + 1; iptr && *iptr; iptr++)
		if ('(' == *iptr)
			++nested_depth;
		else if (')' == *iptr) {
			++nested_depth;
			char	*eptr = iptr;
			for (eptr = iptr; eptr && *eptr && nested_depth; eptr++)
				if (')' == *eptr)
					--nested_depth;
			if (!nested_depth)
				return (eptr - 1);
		}
	errx(EXIT_FAILURE, "Invalid parentheses.");
	return NULL;
}

static inline char	*pth_res_to_str(char *expr) {
	char	*out = NULL;

	assert(out = calloc(25, sizeof(char)));
	sprintf(out, "%lld", choose_op_parser(expr));
	return out;
}

static char	*parse_parentheses(char *expr) {
	char	*pth_start = strchr(expr, '(');
	if (!pth_start)
		return expr;

	char	*pth_end = find_pth_close(pth_start);
	const size_t	e_len = pth_end - pth_start;
	char	*e = NULL;

	assert(e = calloc(e_len, sizeof(char)));
	memcpy(e, pth_start + 1, e_len - 1);
	if (strchr(e, '(')) {
		char	*res = NULL;
		size_t	res_len = 0;

		res = pth_res_to_str(parse_parentheses(e));
		res_len = strlen(res);
		memcpy(pth_start, res, res_len);
		strcpy(expr + res_len, pth_end + 1);
		free(e);
		free(res);
		return expr;
	}
	char	*tmp = pth_res_to_str(e);
	size_t	tmp_len = strlen(tmp);

	strcpy(expr + (pth_start - expr), tmp);
	strcpy(expr + (pth_start - expr) + tmp_len, pth_end + 1);
	free(tmp);
	free(e);
	return expr;
}

static inline expr_t	expr_parser(char *expr) {
	char	*e = parse_parentheses(expr);

	bool	is_op_after_pth_parse = false;
	for (const char *iptr = expr; iptr && *iptr; iptr++)
		if (_is_sym_op_any(*iptr))
			is_op_after_pth_parse = true;

	return (is_op_after_pth_parse ? choose_op_parser(e) : atoll(e));
}

# undef _is_sym_op_any
# undef _is_sym_op_default
# undef _is_sym_op_priority

# undef _skip_digits

static expr_t __attribute__((noreturn))
f_expr_invalid(const expr_t a, const expr_t b) {
	(void)a; (void)b;
	errx(EXIT_FAILURE, "Invalid expression.");
}
static expr_t	f_expr_add(const expr_t a, const expr_t b) { return a + b; }
static expr_t	f_expr_sub(const expr_t a, const expr_t b) { return a - b; }
static expr_t	f_expr_div(const expr_t a, const expr_t b) { return a / b; }
static expr_t	f_expr_mul(const expr_t a, const expr_t b) { return a * b; }

static inline expr_t	expr_run(const struct s_expr_data *restrict ed) {
	static expr_t (*g_expr_op_lt[5])(const expr_t, const expr_t) = {
		f_expr_invalid, f_expr_add, f_expr_sub, f_expr_mul, f_expr_div
	};
	return g_expr_op_lt[ed->op](ed->l_value, ed->r_value);
}

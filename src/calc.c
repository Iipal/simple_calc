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

static expr_t	expr_run(const struct s_expr_data *restrict ed);

typedef expr_t (*fnptr_expr_op_)(const expr_t, const expr_t);
static expr_t __attribute__((noreturn))
		f_expr_invalid(const expr_t a, const expr_t b);
static expr_t	f_expr_add(const expr_t a, const expr_t b);
static expr_t	f_expr_sub(const expr_t a, const expr_t b);
static expr_t	f_expr_div(const expr_t a, const expr_t b);
static expr_t	f_expr_mul(const expr_t a, const expr_t b);

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

static void __attribute__((noreturn))	print_help(void) {
	printf(MSG_USAGE "\n"
	 "Available symbols: " MSG_VALID "\n"
	 "Example: ./calc \"(2 + 4) * 5\" \"(2 + 2)\" \"(2 * 4 + 5) - 3\"\n"
	 "Output:\n30\n4\n10\n");
	_Exit(EXIT_SUCCESS);
}

static char	*trim_whitespaces(const char *src) {
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

static void	syntax_validation_throw(const char *expr) {
# define _is_valid_expr_sym(c) (isdigit((int)(c)) \
	|| '+' == (c) || '-' == (c) || '/' == (c) || '*' == (c) \
	|| '(' == (c) || ')' == (c))

	const char *iptr = expr;
	for (; iptr && *iptr && _is_valid_expr_sym(*iptr); iptr++)
			;
	if (*iptr)
		errx(EXIT_FAILURE, "invalid symbol -- %c\n"
			"valid symbols are: \'" MSG_VALID "\'\n", *iptr);

# undef _is_invalid_expr_sym
}

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

# define _skip_digits(_s) while ((_s) && *(_s) && isdigit(*(_s))) ++(_s);

# define _is_sym_op_priority(c) ('*' == (c) || '/' == (c))
# define _is_sym_op_default(c)  ('+' == (c) || '-' == (c))
# define _is_sym_op_any(c)      ('+' == (c) || '-' == (c) \
						      || '*' == (c) || '/' == (c))

static inline bool	check_signed_lvalue(const char *expr) {
	const char	*iptr = expr;
	if ('-' == *iptr) {
		if (_is_sym_op_any(expr[-1]))
			return true;
		else {
			++iptr;
			_skip_digits(iptr);
			return _is_sym_op_any(*iptr);
		}
	}
	return false;
}

// ltr - left to right
static expr_t	parse_op_def_ltr(const char *expr,
							struct s_expr_data *rec_e) {
	struct s_expr_data	ed = { 0LL, 0LL, e_op_invalid };
	const char	*iptr = expr;

	iptr += check_signed_lvalue(expr);
	if (!rec_e) {
		if (!_is_sym_op_any(*iptr)) {
			_skip_digits(iptr);
			ed.op = get_expr_op(*iptr);
		}
	} else {
		ed.op = rec_e->op;
	}
	if (!rec_e && (!iptr[1] || !isdigit(iptr[1])))
		ed.op = e_op_invalid;
	ed.l_value = rec_e ? rec_e->l_value : atoll(expr);
	ed.r_value = atoll(iptr++ + !rec_e);
	if (0 > ed.r_value)
		++iptr;
	_skip_digits(iptr);
	if (*iptr) {
		ed.l_value = expr_run(&ed);
		ed.op = get_expr_op(*iptr);
		return parse_op_def_ltr(++iptr, &ed);
	}
	return expr_run(&ed);
}

static inline bool	is_expr_has_priority(const char *expr) {
	for (; expr && *expr && !_is_sym_op_priority(*expr); expr++)
		;
	return !!*expr;
}

static expr_t	parse_op_priority(char *expr) {
	const bool	is_signed_lvalue = check_signed_lvalue(expr);
	struct s_expr_data	ed = { 0LL, 0LL, e_op_invalid };
	char	*iptr = expr;
	char	*l_operand = NULL;

	for (; iptr && *iptr && !_is_sym_op_priority(*iptr); iptr++)
		if (isdigit(*iptr) && ((_is_sym_op_default(iptr[-1]))
			|| ('-' == iptr[-1] && _is_sym_op_default(iptr[-2])) || !iptr[-1]))
			l_operand = iptr;
	if (!*iptr || !iptr[1] || !isdigit(iptr[1]) || !l_operand
	|| (_is_sym_op_default(iptr[1]) || _is_sym_op_default(iptr[-1])))
		errx(EXIT_FAILURE, "Invalid expression.");
	l_operand -= (is_signed_lvalue && '-' == l_operand[-1]);
	ed.op = get_expr_op(*iptr);
	ed.l_value = atoll(l_operand);
	ed.r_value = atoll(++iptr);
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
		return (is_expr_has_priority(expr)
			? parse_op_priority(expr)
			: parse_op_def_ltr(expr, NULL));
	}
	return expr_run(&ed);
}

# define _is_pth(c) ('(' == (c) || ')' == (c))

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

static char	*expr_res_to_str(char *expr) {
	expr_t	res = is_expr_has_priority(expr)
				? parse_op_priority(expr)
				: parse_op_def_ltr(expr, NULL);
	char	*str = calloc(25, sizeof(char));

	assert(str);
	sprintf(str, "%lld", res);
	return str;
}

static char	*parse_parentheses(char *expr) {
	char	*pth_start = strchr(expr, '(');
	if (!pth_start)
		return expr;
	char	*pth_end = find_pth_close(pth_start);
	const size_t	e_len = pth_end - pth_start;
	char	*e = calloc(e_len - 1, sizeof(char));

	assert(e);
	memcpy(e, pth_start + 1, e_len - 1);
	if (strchr(e, '(')) {
		char	*res = NULL;
		size_t	res_len = 0;

		res = expr_res_to_str(parse_parentheses(e));
		res_len = strlen(res);
		memcpy(pth_start, res, res_len);
		strcpy(expr + res_len, pth_end + 1);
		free(e);
		free(res);
		return expr;
	}
	char	*tmp = expr_res_to_str(e);
	size_t	tmp_len = strlen(tmp);

	strcpy(expr + (pth_start - expr), tmp);
	strcpy(expr + (pth_start - expr) + tmp_len, pth_end + 1);
	free(tmp);
	return expr;
}

static bool		is_has_op(const char *expr) {
	bool	is_op = false;

	for (const char *iptr = expr; iptr && *iptr; iptr++)
		if (_is_sym_op_any(*iptr)) {
			is_op = true;
			break ;
		}
	return is_op;
}

static expr_t	expr_parser(char *expr) {
	char	*e = parse_parentheses(expr);

	if (!is_has_op(e))
		return atoll(e);
	return (is_expr_has_priority(e)
		? parse_op_priority(e)
		: parse_op_def_ltr(e, NULL));
}

# undef _is_pth

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

static expr_t	expr_run(const struct s_expr_data *restrict ed) {
	static const fnptr_expr_op_	g_expr_op_lt[5] = {
		f_expr_invalid, f_expr_add, f_expr_sub, f_expr_mul, f_expr_div
	};
	return g_expr_op_lt[ed->op](ed->l_value, ed->r_value);
}

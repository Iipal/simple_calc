#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <err.h>
#include <ctype.h>
#include <assert.h>

#define MSG_USAGE "Usage: ./calc \"<expr-1>\" ... \"<expr-N>\""
#define MSG_VALID "1234567890+-/*()"

void __attribute__((noreturn))	print_help(void);
char	*trim_whitespaces(const char *src);
bool	syntax_validation(const char *expr);
long long	expr_parser(const char *expr);

int	main(int argc, char *argv[]) {
	--argc; ++argv;
	if (argc && (!strcmp(*argv, "--help") || !strcmp(*argv, "-h")))
		print_help();
	if (!argc)
		errx(EXIT_FAILURE, MSG_USAGE);

	char		*trimmed_expr = NULL;
	while (argc) {
		printf("\"%s\":\n", *argv);
		assert(trimmed_expr = trim_whitespaces(*argv));
		if (!syntax_validation(trimmed_expr)) {
			free(trimmed_expr);
			return EXIT_FAILURE;
		}
		printf("result = %lld\n", expr_parser(trimmed_expr));
		free(trimmed_expr);
		--argc; ++argv;
	}
}

void __attribute__((noreturn))	print_help(void) {
	printf(MSG_USAGE "\n"
	 "Available symbols: " MSG_VALID "\n"
	 "Example: ./calc \"(2 + 4) * 5\"\n"
	 "Output: \"(2 + 4) * 5\" = 30\n");
	_Exit(EXIT_SUCCESS);
}

char	*trim_whitespaces(const char *src) {
	const size_t	src_len = strlen(src);
	char			*out = NULL;
	size_t			trim_len = 0;
	size_t			j = 0;
	size_t			i = 0;

	for (i = 0; src_len > i; i++)
		if (!isspace(src[i]))
			++trim_len;
	if (!(out = calloc(trim_len + 1, sizeof(char))))
		return NULL;
	for (i = 0; src_len > i; i++)
		if (!isspace(src[i]))
			out[j++] = src[i];
	return out;
}

bool	syntax_validation(const char *expr) {
# define _is_invalid_expr_sym(c) !(isdigit((int)(c)) || \
									'(' == (int)(c) || \
									')' == (int)(c) || \
									'+' == (int)(c) || \
									'-' == (int)(c) || \
									'/' == (int)(c) || \
									'*' == (int)(c))

	for (const char *iptr = expr; iptr && *iptr; iptr++)
		if (_is_invalid_expr_sym(*iptr)) {
			fprintf(stderr, "invalid symbol -- %c\n"
				"valid symbols are: \'" MSG_VALID "\'\n", *iptr);
			return false;
		}
	return true;

# undef _is_invalid_expr_sym
}

long long	expr_parser(const char *expr) {
	long long	res = 0LL;
	return res + !!expr;
}

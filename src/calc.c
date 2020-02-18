#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <err.h>

bool	syntax_parser(const char *expr);
bool	syntax_parser(const char *expr) {
	return !!expr;
}

int	main(int argc, char *argv[]) {
	--argc; ++argv;
	if (!argc)
		errx(EXIT_FAILURE, "Usage: ./calc \"<expr-1>\" ... \"<expr-N>\"");

	while (argc) {
		if (!syntax_parser(*argv))
			return EXIT_FAILURE;
		--argc; ++argv;
	}
}

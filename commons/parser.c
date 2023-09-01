#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "parser.h"

ssize_t writen(int fd, const void *buffer, size_t n)
{
	ssize_t numWritten; /* # of bytes written by last write() */
	size_t totWritten; /* Total # of bytes written so far */
	const char *buf;
	buf = buffer; /* No pointer arithmetic on "void *" */
	for (totWritten = 0; totWritten < n; ) {
		numWritten = write(fd, buf, n - totWritten);
		if (numWritten <= 0) {
			if (numWritten == -1 && errno == EINTR)
				continue; /* Interrupted --> restart write() */
			else
				return -1; /* Some other error */
		}
		totWritten += numWritten;
		buf += numWritten;
	}
	return totWritten; /* Must be 'n' bytes if we get here */
}

int text_parser(char *buf, char *toks[3], int lens[3])
{
//	char *toks[10];
//	int lens[10];
	int ntok;

	// log(3, "pasrse(%s)", buf);

	/* Separar tokens */
	{
		char *p = buf;
		ntok = 0;
		toks[ntok++] = p;
		while (ntok < 10 && (p = strchrnul(p, ' ')) && *p) {
			/* Longitud token anterior */
			lens[ntok-1] = p - toks[ntok-1];
			*p++ = 0;
			/* Comienzo nueva token */
			toks[ntok++] = p;
		}
		lens[ntok-1] = p - toks[ntok-1];
	}

	// log(1, "checking '%s', ntok = %i", toks[0], ntok);
	return ntok;

}



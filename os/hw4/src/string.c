#include "string.h"

void memset(void *ptr, int value, size_t num)
{
    int8_t *p = (int8_t*)ptr;
    for (int i = 0; i < num; i++) {
        *p++ = value;
    }
}

void itoa (char *buf, char base, int d)
{
	char *p = buf;
	char *p1, *p2;
	uint32_t ud = d;
	int divisor = 10;

	/* If %d is specified and D is minus, put `-' in the head. */
	if (base == 'd' && d < 0) {
		*p++ = '-';
		buf++;
		ud = -d;
	} else if (base == 'x') {
		divisor = 16;
	}

	/* Divide UD by DIVISOR until UD == 0. */
	do {
		int remainder = ud % divisor;
		*p++ = (remainder < 10) ? remainder + '0' : remainder + 'a' - 10;
	} while (ud /= divisor);

	/* Terminate BUF. */
	*p = 0;

	/* Reverse BUF. */
	p1 = buf;
	p2 = p - 1;
	while (p1 < p2) {
		char tmp = *p1;
		*p1 = *p2;
		*p2 = tmp;
		p1++;
		p2--;
	}
}
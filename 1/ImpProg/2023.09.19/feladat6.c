#include <stdio.h>

int main () {
	int be;
	int ki;
	printf("A megfordítandó szám: ");
	scanf("%d", &be);

	while (be > 9){		
		int seged = be % 10;
		be -= seged;
		ki += seged;
		ki *= 10;
		be /= 10;
	}
	ki += be;

	printf("A szám megfiordítva: %d\n", ki);

	return 0;
}

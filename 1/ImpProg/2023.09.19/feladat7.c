#include <stdio.h>

int main () {
	int be;
	int i;

	printf("A szám: ");
	scanf("%d", &be);
	printf("A szám osztói: ");

	for (i = 1; i < be + 1; i++){		
		if (be % i == 0){
			printf("%d, ", i);
		}
	}

	printf("\n");

	return 0;
}

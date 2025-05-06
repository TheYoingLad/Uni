#include <stdio.h>

int main () {
	int be1, be2;

	printf("Az első szám: ");
	scanf("%d", &be1);
	printf("A második szám: ");
	scanf("%d", &be2);

	while (be1 != be2){		
		if (be1 > be2){
			be1 -= be2;
		} else {
			be2 -= be1;
		}
	}

	printf("A két szám legnagyobb közös osztója: %d\n", be1);

	return 0;
}

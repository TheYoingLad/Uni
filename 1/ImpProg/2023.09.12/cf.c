#include <stdio.h>

int main () {
	float c;
	float f;

	printf("C fok: ");
	scanf("%f", &c);

	f = c * 1.8 + 32;

	printf("%.2f C° = %.2f F°\n", c, f);

	return 0;
}

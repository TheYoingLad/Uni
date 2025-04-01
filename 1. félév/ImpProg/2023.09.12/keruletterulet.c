#include <stdio.h>

int main() {
	float a;
	float b;
	float r;
	float pi = 3.1415;

	printf("A négyszög egyik oldala: ");
	scanf("%f", &a);

	printf ("A négyszög másik oldala: ");
	scanf("%f", &b);

	printf("A kör sugara: ");
	scanf("%f", &r);
	printf("\n");

	printf("A négyszög kerülete: %.2f\n", a+a+b+b);
	printf("A négyszög területe: %.2f\n\n", a*b);
	printf("A kör kerülete: %.2f\n", 2*r*pi);
	printf("A kör területe: %.2f\n", r*r*pi);

	return 0;
}

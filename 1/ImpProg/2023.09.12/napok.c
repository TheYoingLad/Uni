#include <stdio.h>

int main () {
	int napok;
	int ev;
	int het;
	int seged;

	printf("Napok száma: ");
	scanf("%d", &napok);

	seged = napok;
	ev = napok/365;
	napok -= ev*365;
	het = napok/7;
	napok -= het*7;

	printf("%d nap = %d év, %d hét, %d nap\n", seged, ev, het, napok);

	return 0;
}

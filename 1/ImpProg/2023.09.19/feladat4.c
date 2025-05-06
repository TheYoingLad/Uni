#include <stdio.h>

int main () {
	float c;
	int i;

	for (i = -2; i < 21; i++){		
		c = (i*10 - 32)/1.8;
		printf("%d F° = %.2f C°\n", i*10, c);
	}

	return 0;
}

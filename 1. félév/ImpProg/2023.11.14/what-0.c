#include<stdio.h>
#include<stdlib.h>
int main()
{
	int i, numbers[1];
	numbers[0] = 9;
	free(numbers); // fordítási hiba, csak dinamikus memóriát lehet
	printf("\nStored integers are ");
	printf("\nnumbers[%d] = %d ", 0, numbers[0]);
	return 0;
}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int* sort(int*, int);

int main(){
	int tomb[] = {1,2,3,4,5,6,7,8,9,0};
	int n = sizeof(tomb)/sizeof(tomb[0]);
	int* sorttomb = sort(tomb, n);
	int i;
	for (i = 0; i < n; i++) printf("%d ", tomb[i]);
	printf("\n");
	for (i = 0; i < n; i++) printf("%d ", sorttomb[i]);
	printf("\n");
	free(sorttomb);
	return 0;
}

int* sort(int* tomb, int n){
	int* ki = malloc(sizeof(int)*n);
	int i;
	int j = 0;
	for (i = 0; i < n; i++){
		if(tomb[i] % 2 == 0){
			ki[j] = tomb[i];
			j++;
		}
	}
	for (i = 0; i < n; i++){
		if(tomb[i] % 2 == 1){
			ki[j] = tomb[i];
			j++;
		}
	}
	return ki;
}
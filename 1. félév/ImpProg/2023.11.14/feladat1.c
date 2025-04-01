#include<stdio.h>
#include<stdlib.h>
#include <string.h>

int main(){
	char p[21], ch;
	int i = 0;
	printf("string: ");
	while((ch = getchar()) != '\n' && i < 20){
		p[i] = ch;
		i++;
	}
	p[i] = '\0';
	char* masolat = malloc(sizeof(char)*i);
	int j;
	for(j = 0; j < i; j++){
		masolat[j] = p[j];
	}
	masolat[i] = '\0';
	printf("%s\n", masolat);
	free(masolat);
	return 0;
}
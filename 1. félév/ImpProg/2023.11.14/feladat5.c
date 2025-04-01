#include<stdlib.h>
#include<stdio.h>
#include<string.h>

int main(int argn, char* args[]){
	char* p[atoi(args[1])];
	char be[255];
	int i, j;
	for(i = 0; i < atoi(args[1]); i++){
		scanf("%s", be);
		p[i] = malloc(sizeof(char)*strlen(be));
		for(j = 0; j < strlen(be); j++) p[i][j] = be[j];
	}
	for(--i; i >= 0; i--) printf("\n%s", p[i]);
	printf("\n");
	free(p);
	return 0;
}
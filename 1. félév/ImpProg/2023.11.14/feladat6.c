#include<stdlib.h>
#include<stdio.h>
#include<string.h>

int notend(char*);

int main(int argn, char* args[]){
	char* p[atoi(args[1])];
	char be[255];
	int i, j;
	i = 0;
	do{
		scanf("%s", be);
		p[i] = malloc(sizeof(char)*strlen(be));
		for(j = 0; j < strlen(be); j++) p[i][j] = be[j];
		i++;
	} while(i < atoi(args[1]) && notend(be));
	if (notend(be)) for(--i; i >= 0; i--) printf("\n%s", p[i]);
	else for(i = i - 2; i >= 0; i--) printf("\n%s", p[i]);
	printf("\n");
	free(p);
	return 0;
}

int notend(char* be){
	if(be[0] == 'E' &&
	   be[1] == 'N' &&
	   be[2] == 'D' &&
	   be[3] == '\0') return 0;
	return 1;
}
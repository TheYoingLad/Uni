#include<stdlib.h>
#include<stdio.h>
#include<string.h>

int main(){
	char* p[5];
	char be[100];
	int i, j;
	for(i = 0; i < 5; i++){
		scanf("%s", be);
		p[i] = malloc(sizeof(char)*strlen(be));
		for(j = 0; j < strlen(be); j++) p[i][j] = be[j];
	}
	printf("\n%s\n%s\n%s\n%s\n%s\n", p[4], p[3], p[2], p[1], p[0]);
	return 0;
}
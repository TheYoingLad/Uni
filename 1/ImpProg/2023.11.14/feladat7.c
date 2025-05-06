#include<stdlib.h>
#include<stdio.h>
#include<string.h>

int main(){
	int n = 4;
	int v = 0;
	char** p;
	char be[255];
	int i, j;
	while(1){
		p = malloc(sizeof(char*)*n);
		i = 0;
		while(v || !feof(stdin)){
			v = 0;
			scanf("%s", be);
			if(i == n){
				n *= 2;
				p = realloc(p, sizeof(char*)*n);
			}
			p[i] = malloc(sizeof(char)*strlen(be));
			strcpy(p[i], be);
			i++;
		}
		for(i -= 2; i >= 0; i--){
			printf("\n%s", p[i]);
			free(p[i]);
		}
		free(p);
		printf("\n\n");
		v = 1;
	}
	return 0;
}
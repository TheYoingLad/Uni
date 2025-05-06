#include<stdlib.h>
#include<stdio.h>
#include<string.h>

char* reverse(char*);

int main(){
	char p[101], ch;
	int i = 0;
	printf("string: ");
	while((ch = getchar()) != '\n' && i < 100){
		p[i] = ch;
		i++;
	}
	printf("before: %s\n", p);
	char* p2 = malloc(sizeof(char)*strlen(p));
	p2 = reverse(p);
	printf("after: %s\n", p2);
	free(p2);
	return 0;
}

char* reverse(char* be){
	int n = strlen(be);
	char* seged = malloc(sizeof(char)*n);
	int i;
	for(i = 0; i < n; i++){
		seged[i] = be[n-i-1];
	}
	return seged;
}
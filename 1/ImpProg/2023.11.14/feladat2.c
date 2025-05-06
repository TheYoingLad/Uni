#include<stdlib.h>
#include<stdio.h>
#include<string.h>

void reverse(char*);

int main(){
	char p[101], ch;
	int i = 0;
	printf("string: ");
	while((ch = getchar()) != '\n' && i < 100){
		p[i] = ch;
		i++;
	}
	printf("before: %s\n", p);
	reverse(p);
	printf("after: %s\n", p);
	return 0;
}

void reverse(char* be){
	int n = strlen(be);
	char* seged = malloc(sizeof(char)*n);
	int i;
	for(i = 0; i < n; i++){
		seged[i] = be[n-i-1];
	}
	for(i = 0; i < n; i++){
		be[i] = seged[i];
	}
}
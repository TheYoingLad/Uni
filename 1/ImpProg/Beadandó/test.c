#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(){
    char a[100];
    fgets(a, 100, stdin);
    int i = strlen(a);
    printf("%d\n", a[i]);
    printf("%d\n", a[i-1]);
    printf("%d\n", a[i-2]);
    /*FILE *fp = fopen("test.txt", "r");
    char *line = malloc(sizeof(char)*1000);
    fgets(line, 1000, fp);
    int n = 0;
    char *next = strtok(line, ";");
    while(next){
        if(next[0] != 13) n++;
        next = strtok(NULL, ";");
    }
    printf("%d\n", n);
    n = 3;
    rewind(fp);
    fgets(line, 1000, fp);
    next = strtok(line, ";");
    while(next){
        printf("%*d ", n, atoi(next));
        next = strtok(NULL, ";");
    }*/
    return 0;
}
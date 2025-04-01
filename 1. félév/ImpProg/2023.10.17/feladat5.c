#include <stdio.h>
#include <stdlib.h>

int main(int n, char *args[]){
    int a = atoi(args[1]);
    int sum = 0;
    int b = atoi(args[2]);
    int i;
    for (i = 0; i++ < b; sum+=a) ;
    printf("%d\n", sum);
    return 0;
}
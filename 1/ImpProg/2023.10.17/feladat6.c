#include <stdio.h>

int fact (int);

int main(){
    printf("number: ");
    int n;
    scanf("%d", &n);
    printf ("factorial: %d\n", fact(n));
    return 0;
}

int fact (int n){
    if (n < 2) return 1;
    return n * fact (n-1);
}
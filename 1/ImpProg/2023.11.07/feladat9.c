#include <stdio.h>

int* big(int*, int*);

int main (){
    int x = 10;
    int y = 5;
    int* a = &x;
    int* b = &y;
    int* c = big(a, b);
    printf("x = %d, y = %d, big = %d\n", *a, *b, *c);
    return 0;
}

int* big(int* a, int* b){
    if (*a > *b) return a;
    else return b;
}
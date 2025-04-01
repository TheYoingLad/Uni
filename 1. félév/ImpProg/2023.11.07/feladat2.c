#include <stdio.h>

int* f(int);

int main (){
    int x = 5;
    int* p = f(x);
    printf("%d\n", *p);
    return 0;
}

int* f(int be){
    static int j = 15;
    j += be;
    return &j;
}
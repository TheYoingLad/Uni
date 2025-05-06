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
    if (j % 2 == 0) j += be;
    else if (j % 3 == 0) j +=3;
    else j += 4; 
    return &j;
}
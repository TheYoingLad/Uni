#include <stdio.h>

void swap(int*, int*);

int main (){
    int x = 1;
    int y = 5;
    int* a = &x;
    int* b = &y;
    printf("x = %d, y = %d\n", x, y);
    swap(a, b);
    printf("x = %d, y = %d\n", x, y);
    return 0;
}

void swap(int* a, int* b){
    int seg = *a;
    *a = *b;
    *b = seg;
}
#include <stdio.h>

void csere (int*, int*);

int main() {
    int a = 5;
    int b = 6;
    int tomb[] = {1, 5};
    printf("a: %d, b: %d\n", a, b);
    csere (&a, &b);
    printf("a: %d, b: %d\n\n", a, b);
    printf("t1: %d, t2: %d\n", tomb[0], tomb[1]);
    csere (tomb, tomb+1);
    printf("t1: %d, t2: %d\n\n", tomb[0], tomb[1]);
    return 0;
}

void csere (int *pa, int *pb){
    int a = *pa;
    *pa = *pb;
    *pb = a;
}
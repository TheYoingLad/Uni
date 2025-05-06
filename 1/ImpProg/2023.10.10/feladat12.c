#include <stdio.h>

int *kisebb (int*, int*);

int main() {
    int *p1, *p2;
    int tomb[] = {9,8,7,6,5,4,3,2,1,0};
    p1 = &tomb[6];
    p2 = &tomb[5];
    int *ki = kisebb(p1, p2);
    if (ki == p1) printf("p1 előbb van mint p2\n");
    else printf("p2 előbb van mint p1\n");
    return 0;
}

int *kisebb (int *a, int *b){
    if (a-b<0) return a;
    else return b;
}
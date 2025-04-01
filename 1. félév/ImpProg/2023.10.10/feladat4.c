#include <stdio.h>

int main() {
    int a = 5;
    int b = 0;
    int *p = &a;
    int **pp = &p;
    *pp = &b;
    *p = 3;
    printf("%d\n", **pp);
    printf("%d\n", *p);
    return 0;
}
#include <stdio.h>

int main() {
    int n = 5;
    int *p = &n;
    *p = 3;
    printf("%d\n", n);
    return 0;
}
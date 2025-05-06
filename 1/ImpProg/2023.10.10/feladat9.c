#include <stdio.h>

int* ptp();

int main() {
    int *p = ptp();
    printf("%d\n", *p);
    return 0;
}

int* ptp(){
    int a = 5;
    int *out = &a;
    return out;
}
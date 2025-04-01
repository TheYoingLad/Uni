#include <stdio.h>
#include "feladat4.h"

int a;

int main(){
    a = 15;
    int s = square();
    printf("%d négyzete %d\n", a, s);
    return 0;
}
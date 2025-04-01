#include <stdio.h>
#include <limits.h>

int main (){
    int imax = INT_MAX;
    unsigned int umax = UINT_MAX;
    
    printf("int: %d\n", sizeof(int));
    printf("unsigned int: %d\n", sizeof(unsigned int));
    printf("long: %d\n", sizeof(long));
    printf("short: %d\n", sizeof(short));
    printf("float: %d\n", sizeof(float));
    printf("double: %d\n", sizeof(double));
    printf("long double: %d\n", sizeof(long double));

    printf("%i\n", imax+1);
    printf("%u\n", umax+1);

    return 0;
}
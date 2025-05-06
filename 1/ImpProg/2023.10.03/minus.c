#include <stdio.h>

unsigned int minus (unsigned int);

int main (){
    unsigned int be;
    scanf("%d", &be);
    be = minus(be);
    printf("%d\n", be);

    return 0;
}

unsigned int minus (unsigned int negate){
    negate *= (-1);
    return negate;
}
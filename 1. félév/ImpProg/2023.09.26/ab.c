#include <stdio.h>

int a (int, int);
int b (int, int);

int main (){
    int be;
    int n;
    printf("Kérek egy számot! ");
    scanf("%d", &be);

    n = a(be, 0);

    printf("%d iteráción ment keresztül.\n", n);

    return 0;
}

int a (int be1, int db1){
    be1 = be1 / 2;    
    return (be1 > 0? b(be1, ++db1):db1);
}

int b (int be2, int db2){
    be2--;
    return (be2 > 0? a(be2, db2):db2);
}
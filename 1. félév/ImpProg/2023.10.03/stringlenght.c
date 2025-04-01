#include <stdio.h>
#define hossz 100

int main (){
    char be[hossz];
    scanf("%s", be);
    int n = 0;
    while (be[n] != '\0' && n < hossz){
        n++;
    }
    printf("ennek a hossza: %d\n", n > hossz?(--n):(n));

    return 0;
}
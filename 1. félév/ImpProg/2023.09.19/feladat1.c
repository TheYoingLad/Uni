#include <stdio.h>

int main (){
    int kezdo;
    printf("%d\n", kezdo);
    scanf("%d", &kezdo);

    if (kezdo % 2 == 0){
        printf("Páros\n");
    } else {
        printf("Páratlan\n");
    }
    if (kezdo > 0){
        printf("Pozitív\n");
    } else if (kezdo < 0)
    {
        printf("Negatív\n");
    } else {
        printf("Nulla\n");
    }

    return 0;
}
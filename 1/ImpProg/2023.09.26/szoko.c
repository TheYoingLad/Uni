#include <stdio.h>

int main (){
    int be;
    
    printf("Vizsgálandó év: ");
    scanf("%d", &be);

    if (be % 4 == 0 && be % 100 != 0) {printf("Szökőév!\n");}
    else if (be % 400 == 0) {printf("Szökőév!\n");}
    else printf("Nem szökőév!\n");

    return 0;
}
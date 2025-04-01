#include <stdio.h>

int main(){
    int tomb[] = {1,23,4,23,1,52,23,123,6,23,1,51,123,5,123,55,131,8};
    int size = sizeof(tomb)/sizeof(tomb[0]);
    int i = 0;
    int keresett;
    printf("keresett szám: ");
    scanf("%d", &keresett);
    do{
        if (tomb[i] == keresett) break;
    }while(++i < size);
    if (i < size) printf("ennek indexe: %d\n", i);
    else printf("nincs benne a listában!\n");
    return 0;
}
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define hossz 10

int main (){
    time_t t;
    srand((unsigned) time(&t));
    int a[hossz];
    int i;
    for(i = 0; i < hossz; i++){
        a[i] = rand() % 100;
    }
    int max = a[0];
    int min = a[0];
    int maxind = 0;
    int minind = 0;
    for (i = 0; i < hossz; i++){
        if (a[i] > max){
            max = a[i];
            maxind = i;
        }
        if (a[i] < min){
            min = a[i];
            minind = i;
        }
    }
    for (i = 0; i < hossz; i++){
        printf("%d, ", a[i]);
    }
    printf("\n");
    a[maxind] = min;
    a[minind] = max;
    for (i = 0; i < hossz; i++){
        printf("%d, ", a[i]);
    }
    printf("\n");

    return 0;
}
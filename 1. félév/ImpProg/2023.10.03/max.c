#include <stdio.h>

int main (){
    int be[] = {5, 1, 123, 3, 234234, 4, 6234};
    int i;
    int max = be[0];
    int min = be[0];
    int min2 = be[0];

    for (i = 0; i < sizeof(be)/sizeof(be[0]); i++){
        if (max < be[i]) {max = be[i];}
    }
    printf("Max: %d\n", max);
    for (i = 0; i < sizeof(be)/sizeof(be[0]); i++){
        if (min > be[i]) {min = be[i];}
    }
    printf("Min: %d\n", min);
    for (i = 0; i < sizeof(be)/sizeof(be[0]); i++){
        if (min2 > be[i] && be[i] != min) {min2 = be[i];}
    }
    printf("Min 2: %d\n", min2);
    return 0;
}
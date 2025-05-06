#include <stdio.h>

int *max(int[], int);

int main() {
    int n[] = {1,2,3,4,5,6,7,6,5,4,3,100,10,2,1};
    int m[] = {10};
    int size = sizeof(n)/sizeof(n[0]);
    int *maxind = max(n, size);
    int *felmaxind = max(n, size/2);
    int *mmax = max(m, 1);
    printf("egésznek %d a legnagyobb eleme\n", *maxind);
    printf("felének %d a legnagyobb eleme\n", *felmaxind);
    printf("1 eleműnek %d a legnagyobb eleme\n", *mmax);
    return 0;
}

int *max(int be[], int size){
    int *ki = be;
    int i;
    for (i = 0; i < size; i++){
        if (*ki < be[i]) ki = &be[i];
    }
    return ki;
}
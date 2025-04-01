#include <stdio.h>
#include <stdlib.h>
#include <time.h>
double med(int[], int);

int main(){
    int size;
    double median;
    do{
        printf("Size of the set: ");
        scanf("%d", &size);
        if (size <= 0) printf("invalid!\n");
    }while (size <= 0);
    int tomb[size];
    int i;
    time_t t;
    srand((unsigned) time(&t));
    for (i = 0; i < size; i++) tomb[i] = rand() % 1000;
    median = med (tomb, size);
    printf("Median: %0.2lf\n", median);
    return 0;
}

double med (int tomb[], int size){
    int i, j, seged;
    for (i = 0; i < size - 1; i++){
        j = i;
        seged = tomb[i];
        while (i < size - 1 && seged > tomb[i + 1]){
            seged = tomb[i];
            tomb[i] = tomb[i + 1];
            tomb[i + 1] = seged;
            i++;
        }
        if (j != i) i = 0;
    }
    if (size % 2 == 1) return (double)tomb[(size - 1) / 2];
    else return (double)(tomb[size / 2] + tomb[size / 2 - 1])/2;
}
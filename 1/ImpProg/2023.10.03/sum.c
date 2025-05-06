#include <stdio.h>

int main (){
    const int a = 20;
    int tomb[a];
    float suly[a];
    int i, sum;
    float wsum, avg;

    for (i = 0; i < a; i++){
        tomb[i] = i;
        printf("%3d ", i);
    }
    printf("\n");
    for (i = 0; i < a; i++){
        suly[i] = (float) i / 8;
        printf("%1.1f ", (float) i / 8);
    }
    printf("\n");
    printf("\n");    
    sum = 0;
    for (i = 0; i < a; i++){
        sum += tomb[i];        
    }
    wsum = 0;
    for (i = 0; i < a; i++){
        wsum += tomb[i]*suly[i];        
    }
    avg = (double) sum / a;
    printf("Sum: %d\n", sum);
    printf("WSum: %.3f\n", wsum);
    printf("Avg: %.3f\n", avg);
    return 0;
}
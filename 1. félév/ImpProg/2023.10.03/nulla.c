#include <stdio.h>

int main (){
    const int a = 10;
    const int b = 15;
    int tomb[a][b];
    int i, j;

    for (i = 0; i < a; i++){
        for (j = 0; j < b; j++){
            tomb[a][b] = 0;
            printf("0 ");
        }
        printf("\n");
    }
    return 0;
}
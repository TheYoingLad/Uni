#include <stdio.h>
#include <stdlib.h>

int** msz(int**, int**, int, int, int);
void freeAll(int**, int);

int main(){
    int m, n, p, i, j;
    printf("A = m x n, B = n x p => AB = m x p\n");
    printf("m = ");
    scanf("%d", &m);
    printf("n = ");
    scanf("%d", &n);
    printf("p = ");
    scanf("%d", &p);
    int** A = malloc(sizeof(int*)*m);
    int** B = malloc(sizeof(int*)*n);
    printf("\nA mátrix:\n");
    for(i = 0; i < m; i++) {
        A[i] = malloc(sizeof(int)*n);
        for(j = 0; j < n; j++){
            printf("%d. sor %d. eleme = ", i+1, j+1);
            scanf("%d", &A[i][j]);
        }
    }
    printf("\nB mátrix:\n");
    for(i = 0; i < n; i++) {
        B[i] = malloc(sizeof(int)*n);
        for(j = 0; j < p; j++){
            printf("%d. sor %d. eleme = ", i+1, j+1);
            scanf("%d", &B[i][j]);
        }
    }
    printf("\n");
    int** C = msz(A, B, m, n, p);
    for(i = 0; i < n; i++){
        for(j = 0; j < n; j++) printf("    ");
        printf(" ");
        for(j = 0; j < p; j++) printf("%3d ", B[i][j]);
        printf("\n");
    }
    printf("\n");
    for(i = 0; i < m; i++){
        for(j = 0; j < n; j++) printf("%3d ", A[i][j]);
        printf(" ");
        for(j = 0; j < p; j++) printf("%3d ", C[i][j]);
        printf("\n");
    }
    printf("\n\nTehát AB =\n\n");
    for(i = 0; i < m; i++){
        for(j = 0; j < p; j++) printf("%3d ", C[i][j]);
        printf("\n");
    }
    printf("\n");
    freeAll(A, m);
    freeAll(B, n);
    freeAll(C, m);
    return 0;
}

int** msz(int** A, int** B, int m, int n, int p){
    int** C = malloc(sizeof(int*)*m);
    int i, j, k;
    for(i = 0; i < m; i++){
        C[i] = malloc(sizeof(int)*p);
        for(j = 0; j < p; j++){
            int s = 0;
            for(k = 0; k < n; k++){
                s += A[i][k]*B[k][j];
            }
            C[i][j] = s;
        }
    }
    return C;
}

void freeAll(int** T, int n){
    int i;
    for(i = 0; i < n; i++){
        free(T[i]);
    }
    free(T);
}
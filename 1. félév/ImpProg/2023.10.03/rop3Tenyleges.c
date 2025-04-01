//ez a helyes megoldás, rossz mentés előtt töltöttem fel az előzőt

#include <stdio.h>

int main () {
    int N, M;
    int i;

    printf("N = ");
    scanf("%d", &N);
    printf("M = ");
    scanf("%d", &M);

    if (N > M) {return 0;}
    printf("Páros számok:\n");
    for (i = N; i <= M;) {
        if (i % 2 == 0) {printf("%d\n", i++);}
        else {i++;}
    }
    printf("\n");
    printf("7-tel osztható számok:\n");
    for (i = M; i >= N; i--) {
        if (i % 7 == 0) {printf("%d\n", i);}
    }

    return 0;
}
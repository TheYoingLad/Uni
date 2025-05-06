#include <stdio.h>
#define larger(a,b) (a==b?(0):(a>b?(1):(-1)))

int main(){
    int a, b;
    printf("a = ");
    scanf("%d", &a);
    printf("b = ");
    scanf("%d", &b);
    int c = larger(a,b);
    switch (c){
        case 0:
            printf("%d = %d\n", a, b);
            break;
        case -1:
            printf("%d < %d\n", a, b);
            break;
        case 1:
            printf("%d > %d\n", a, b);
            break;
    }
    return 0;
}
#include <stdio.h>
#include <stdlib.h>

int sum (int, int);
int prod (int, int);
int fnc (int, int(*fp)(int, int));
int n;
int* v;

int main(){
    printf("n = ");
    scanf("%d", &n);
    v = malloc(sizeof(int)*n);
    int i;
    for(i = 0; i < n; i++){
        printf("%d. komponens = ", i+1);
        scanf("%d", &v[i]);
    }
    printf("vektor: (");
    for(i = 0; i < n-1; i++){
        printf("%d, ", v[i]);
    }
    printf("%d)\n", v[n-1]);
    int (*sp)(int, int) = sum;
    int s = fnc(0, sp);
    int (*pp)(int, int) = prod;
    int p = fnc(0, pp);
    printf("sum = %d\n", s);
    printf("prod = %d\n", p);
    free(v);
    return 0;
}

int fnc (int i, int(*fp)(int, int)){
    int s = v[0];
    for (i++ ; i < n; i++){
        s = fp(s, v[i]);
    }
    return s;
}

int sum (int a, int b){
    return a+b;
}
int prod (int a, int b){
    return a*b;
}
#include <stdio.h>
#define max 100 
int hossz (char[]);
int order (char[], char[], int, int);

int main (){
    char be1[max];
    char be2[max];
    int n1, n2, res;

    scanf("%s", be1);
    scanf("%s", be2);
    n1 = hossz(be1);
    n2 = hossz(be2);

    res = order (be1, be2, n1, n2);
    if(res == 2) printf("Ugyan az a két szó!\n");
    else if(res == 1) printf("A helyes sorrend: %s, %s\n", be1, be2);
    else printf("A helyes sorrend: %s, %s\n", be2, be1);
    return 0;
}

int hossz (char be[]){
    int n = 0;
    while (be[n] != '\0' && n < max){
        n++;
    }
    return (n > max?(--n):(n));
}

int order (char in1[], char in2[], int h1, int h2){
    int ki = 1;
    int i = 0;
    int h = h1;
    int rem = 0;
    if (h2 > h1) {
        h = h2;
        for (i = h1;i<h;i++){
        in1[i] = '0';
        rem = 1;
        }
    }else {
        for (i = h2;i<h;i++){
        in2[i] = '0';
        }
    }
    i = 0;
    while (in1[i] == in2[i] && i < h){
        i++;
    };
    if (i == h) return 2;
    
    if (rem == 0) in2[h2] = '\0';
    else in1[h1] = '\0';
    
    if (in1[i] < in2[i]) return 1;
    else return 0;
}
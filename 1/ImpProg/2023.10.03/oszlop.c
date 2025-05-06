#include <stdio.h>
#define size 100

int hossz (char[]);
int pow26 (int);

int main(){
    char be[size];
    int i, jo, n;
    do {
        jo = 1;
        scanf("%s", be);
        n = hossz(be);
        for (i = 0; i < n; i++) if (be[i] < 'A' || be[i] > 'Z') jo = 0;
        if (jo == 0) printf("Helytelen bemenet!\n");
    } while (jo == 0);
    int hat = n-1;
    int oszlop = 0;
    for (i = 0; i < n; i++) oszlop += (be[i]-('A' - 1))*pow26(hat--);
    printf("%d\n", oszlop);
    return 0;
}

int hossz (char be[]){
    int i = 0;
    while (i < size && be[i] != '\0') i++;
    return i;
}

int pow26(int hat){
    int out = 1;
    int i;
    for (i = 0; i < hat; i++) out *= 26;
    return out;
}
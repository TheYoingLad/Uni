#include <stdio.h>

int main(){
    char be[20];
    int i = 0, n;
    do{
        scanf("%c", &be[i++]);
    }while (i < 20 && be[i-1] != '\n');
    n = i;
    i = 0;
    while (i < 20 && be[i] != '\0'){
        if(be[i]>= 'a' && be[i]<= 'z') be[i] = be[i]-'a'+'A';
        else if (be[i] == '1' || be[i] == '3' || 
                 be[i] == '5' || be[i] == '7' || be[i] == '9') be[i] -= 1;
        else if (be[i] == ' ') be[i] = '\n';
        i++;
    }
    i = 0;
    while (i < n) printf("%c", be[i++]);
    return 0;
}
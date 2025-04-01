#include <stdio.h>

void toLowerCase(char*);

int main(){
    char szoveg[] = "induL a gOrog aLudni";
    printf("%s\n", szoveg);
    toLowerCase (szoveg);
    printf("%s\n", szoveg);
    return 0;
}

void toLowerCase(char* be){
    int i = 0;
    while(be[i] != '\0'){
        if (be[i] <= 'A' || be[i] >= 'Z') {i++; continue;}
        printf("%c to %c\n", be[i], be[i]+32);
        be[i++] += 32;
    }
}
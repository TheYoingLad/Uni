#include <stdio.h>

char change1(char);
char change2(char);
char change3(char);
char change4(char);

int main(){
    char szoveg[] = "indul a gorog aludni";
    int i = 0;
    printf("%s\n", szoveg);
    for ( ; szoveg[i] != '\0';i++) szoveg[i] = change1(szoveg[i]);
    printf("%s\n", szoveg);
    i = 0;
    while (szoveg[i] != '\0') {szoveg[i] = change2(szoveg[i]); i++;}
    printf("%s\n", szoveg);
    i = 0;
    do{ szoveg[i] = change3(szoveg[i]); } while (szoveg[++i] != '\0');
    printf("%s\n", szoveg);
    for (i = 0; szoveg[i] != '\0';i++) szoveg[i] = change4(szoveg[i]);
    printf("%s\n", szoveg);
    return 0;
}

char change1 (char be){
    if(be == 'a') return 'e';
    else if(be == 'e') return 'i';
    else if(be == 'i') return 'o';
    else if(be == 'o') return 'u';
    else if(be == 'u') return 'a';
    else return be;
}

char change2 (char be){
    switch(be){
        case 'a': return 'e';
        case 'e': return 'i';
        case 'i': return 'o';
        case 'o': return 'u';
        case 'u': return 'a';
        default: return be;
    }
}

char change3 (char be){
    char a[] = "aeiou";
    char b[] = "eioua";
    int i = 0;
    for (; i < 5; i++) if (a[i] == be) return b[i];
    return be;
}

char change4 (char be){
    char a[] = "aeiou";
    int i = 0;
    for (; i < 5; i++) if (a[i] == be) return a[(i+1) % 5];
    return be;
}
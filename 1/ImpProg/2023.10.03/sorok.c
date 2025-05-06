#include <stdio.h>
#define hossz 100

int main(){
    char be[hossz];
    scanf("%s", be);
    int i;
    int db = 1;
    for(i = 0; i< hossz; i++) if(be[i] == '\n') db++;
    printf("Sorok száma: %d\n", db);
    return 0;
}
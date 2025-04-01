#include <stdio.h>
#define hossz 100

int main(){
    char be[hossz];
    scanf("%s", be);
    int i = 0;
    int mghdb = 0;
    int mshdb = 0;
    char mgh[] = "aeiou";
    char msh[] = "bcdfghjklmnpqrstvwxyz";
    while (be[i] != '\0' && i < hossz){
        int j;
        for (j = 0; j < sizeof(mgh)/sizeof(mgh[0])-1; j++) if(be[i] == mgh[j]) mghdb++;
        for (j = 0; j < sizeof(msh)/sizeof(msh[0])-1; j++) if(be[i] == msh[j]) mshdb++;
        i++;
    }
    printf("%d darab mgh\n%d darab msh\n", mghdb, mshdb);
    return 0;
}
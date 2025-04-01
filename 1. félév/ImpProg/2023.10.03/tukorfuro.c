#include <stdio.h>

int main(){
    char be[] = "árvíztűrőtükörfúrógép";
    printf("%d\n", sizeof(be)/sizeof(be[0])-1);
    return 0;
}
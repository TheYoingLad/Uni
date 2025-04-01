#include <stdio.h>

int main (){
    int i;
    int* b;
    for (i = 0; i< 5 ;i++){
        static int x = 5;
        x++;
        b = &x;
    }
    printf("%d\n", *b);
    return 0;
}
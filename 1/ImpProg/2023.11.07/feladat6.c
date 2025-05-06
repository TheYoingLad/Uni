#include <stdio.h>

int add (int, int);

int main (){
    int x = 0;
    int y = 1;
    int z = add (x, y);
    printf("%d\n", x);
    return 0;
}

int add (int x, int x){
    return x + x;
    // x 2szer van def-va!
}
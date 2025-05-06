#include <stdio.h>

int sum (int*, int);

int main() {
    int tomb[] = {1,2,3,4,5,6,7,8,9,10};
    int s = sum (tomb, 10);
    printf("%d\n", s);
    return 0;
}

int sum (int* tomb, int hossz){
    int i;
    int out = 0;
    for (i = 0; i < hossz; i++){
        out += *(tomb+i);
    }
    return out;
}
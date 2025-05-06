#include <stdio.h>

int sum (int*, int*);

int main() {
    int tomb[] = {1,2,3,4,5,6,7,8,9,10};
    int s = sum (tomb, tomb+10);
    printf("%d\n", s);
    return 0;
}

int sum (int *eleje, int *vege){
    int out = 0;
    int i = 0;
    while (eleje+i != vege) {
        out += *(eleje+i);
        i++;
    } 
    return out;
}
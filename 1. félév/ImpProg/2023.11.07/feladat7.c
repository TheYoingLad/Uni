#include <stdio.h>

void inc();

int main(){
    inc();
    inc();
    inc();
    inc();
    inc();
    inc();
    return 0;
}

void inc(){
    static int i = 0;
    printf("%d\n", i++);
}
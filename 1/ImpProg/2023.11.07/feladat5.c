#include <stdio.h>

int x = 0;

int main (){
    {
        int a = 5;
        {
            int b = 4;
            printf("%d\n", a);
            printf("%d\n", b);
        }
        printf("%d\n", a);
        //prinf("%d\n", b);
    }
    return 0;
}
#include <stdio.h>
#define printOkos(x) (printf(#x"\n"))

int main(){
    printOkos(15);
    printOkos("alma");
    return 0;
}
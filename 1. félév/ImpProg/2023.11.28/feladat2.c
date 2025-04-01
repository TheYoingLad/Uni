#include <stdio.h>

int main(){
    struct Student1{
        double avg;
        int az;
        int kor;
    };
    typedef struct Student1 alias;
    printf("student1: %d\n", sizeof(alias));
    alias Jani = {5.0,242,14};
    printf("avg: %.1lf, az: %d, age:%d\n", Jani.avg, Jani.az, Jani.kor);
    return 0;
}
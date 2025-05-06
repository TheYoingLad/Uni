#include <stdio.h>
#include <stdbool.h>

int main (){
    bool egy = false;
    bool ketto = false;
    bool harom = false;
    int a = 1;
    int b = 2;
    int c = 3;

    egy = b < b < b;
    ketto = 2 < 2 < 2;
    harom = 2 < b < a < 0 < 123 < -3 < c;

    printf("%s\n", egy? "True":"False");
    printf("%s\n", ketto? "True":"False");
    printf("%s\n", harom? "True":"False");
    
    return 0;
}
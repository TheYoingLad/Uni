#include <stdio.h>

void liwe (int, char*);

int main(){
    liwe (0, "édesapám");
    liwe (1, "maga felé dől a fa");
    liwe (2, "mit beszél maga?");
    liwe (15, "székley has died");
    return 0;
}

void liwe (int a, char *b){
    switch (a){
        case 0:{
            printf("INFO - ");
            break;
        }
        case 1:{
            printf("WARNING - ");
            break;
        }
        case 2:{
            printf("ERROR - ");
            break;
        }
        default:{
            printf("LOG - ");
            break;
        }
    }
    printf("%s\n", b);
}
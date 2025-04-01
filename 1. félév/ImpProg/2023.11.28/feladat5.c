#include <stdio.h>

typedef struct{
    enum {BSc, MSc, PhD} Type;
    int az;
    int kor;
    double avg;
    union {int kurzusdb; double kreditindex; struct {double pubimp; int erdos;} phdbigyo;};
} alias;

typedef struct Student{
    enum {BSc2, MSc2, PhD2} Type2;
    int az;
    int kor;
    double avg;
    struct {int kurzusdb; double kreditindex; struct {double pubimp; int erdos;} phdbigyo;};
} alias2;

int main(){
    printf("elso: %d\n", sizeof(alias));
    printf("masodik: %d\n", sizeof(alias2));
    return 0;
}
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#define SIZE 10

typedef enum Type {BSc, MSc, PhD} Type;

typedef struct{
    Type fejlettseg;
    int az;
    int kor;
    double avg;
    union {int kurzusdb; double kreditindex; struct {double pubimp; int erdos;} phdbigyo;} tipusbigyo;
} alias;

alias* student_init(Type);
void nyomtat(alias*);
alias* legjobb(alias**);
void freeAll(alias**);

int main(){
    srand(time(NULL));
    alias** osztaly = malloc(sizeof(alias*)*SIZE);
    int i;
    for(i = 0; i < SIZE; i++) osztaly[i] = student_init(rand() % 3);
    alias* best = legjobb(osztaly);
    printf("Legjobb tanuló:\n");
    nyomtat(best);
    freeAll(osztaly);
    return 0;
}

alias* student_init(Type tipus){
    alias* delikvens = malloc(sizeof(alias));;
    delikvens->az = rand() % 100;
    delikvens->kor = 20 + rand() % 10;
    delikvens->avg = (double)(rand() % 401) / 100 + 1;
    switch (tipus){
        case BSc:
            delikvens->tipusbigyo.kurzusdb = rand() % 20;
            delikvens->fejlettseg = BSc;
            break;
        case MSc:
            delikvens->tipusbigyo.kreditindex = (double)(rand() % 300) / 10;
            delikvens->fejlettseg = MSc;
            break;
        case PhD:
            delikvens->tipusbigyo.phdbigyo.pubimp = (double)(rand() % 1000) / 1000;
            delikvens->tipusbigyo.phdbigyo.erdos = rand() % 6;
            delikvens->fejlettseg = PhD;
            break;
    }
    return delikvens;
}

void nyomtat(alias* tanulo){
    printf("Azonosító: %d\n", tanulo->az);
    printf("Kor: %d\n", tanulo->kor);
    printf("Átlag: %.2lf\n", tanulo->avg);
    switch (tanulo->fejlettseg){
        case BSc:
            printf("Fejlettség: BSc\n");
            printf("Felvett kurzusok száma: %d\n", tanulo->tipusbigyo.kurzusdb);
            break;
        case MSc:
            printf("Fejlettség: MSc\n");
            printf("Korrigált kreditindex: %.2lf\n", tanulo->tipusbigyo.kreditindex);
            break;
        case PhD:
            printf("Fejlettség: PhD\n");
            printf("Impakt: %.2lf\n", tanulo->tipusbigyo.phdbigyo.pubimp);
            printf("Erdős: %d\n", tanulo->tipusbigyo.phdbigyo.erdos);
            break;
    }
}

alias* legjobb(alias** osztaly){
    double max = osztaly[0]->avg; 
    int index = 0;
    int i;
    for(i = 1; i < SIZE; i++){
        if(osztaly[i]->avg>max){
            max = osztaly[i]->avg;
            index = i;
        }
    }
    return osztaly[index];
}

void freeAll(alias** osztaly){
    int i;
    for(i = 0; i < SIZE; i++) free(osztaly[i]);
    free(osztaly);
}
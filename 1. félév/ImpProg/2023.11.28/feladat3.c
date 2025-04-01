#include <stdio.h>
#define SIZE 4

typedef struct Student{
    int az;
    int kor;
    double avg;
} tanulo;
int legjobb(tanulo[SIZE]);

int main(){
    tanulo hetedik[]={ {1,10,2.0}, {2,10,4.5}, {3,11,4.8}, {4,11,3.9} };
    int best = legjobb(hetedik);
    printf("legjobb átlag azonosító: %d\n", best);
    return 0;
}

int legjobb (tanulo osztaly[SIZE]){
    double max = osztaly->avg;
    int az = osztaly->az;
    int i;
    for(i = 1; i < SIZE; i++){
        if((osztaly+i)->avg>max){
            max = (osztaly+i)->avg;
            az = (osztaly+i)->az;
        }
    }
    return az;
}
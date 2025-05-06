#include <stdio.h>
#include <stdlib.h>
#include <time.h>

char get_target();
int guessing (char);
int evaluate (int);

int main (){
    char n;
    int db;

    /*srand((unsigned) time(&t));
    n = (char) rand();

    printf("Gondoltam egy számra!\n");
    do {
        printf("Tippelj! ");
        scanf("%d", &tipp);
        if (tipp > n) {printf("Túl nagy!\n");}
        else if (tipp < n) {printf("Túl kicsi!\n");}        
        db++;
    } while(tipp != n);
    
    printf("Kitaláltad, a szám %d volt, ehhez %d tipp kellett!\n", n, db);*/


    /*for (printf("Gondoltam egy számra!\n"); tipp-n; n>tipp? (tipp==0? printf("Feladtad! A helyes szám: %d", n):printf("Túl kicsi!\n")):(n==tipp? printf("Kitaláltad, a szám %d volt!\n", n):printf("Túl nagy!\n"))) {scanf("%d", &tipp);}*/

    n = get_target();
    db = guessing(n);
    evaluate(db);

    return 0;
}

char get_target (){
    time_t t;
    srand((unsigned) time(&t));
    char randomly = rand();
    while (randomly < 0) {randomly = rand();}

    return randomly;
}

int guessing (char be){
    int darab = 00;
    int tipp;
    printf("Gondoltam egy számra!\n");
    do {
        printf("Tippelj! ");
        scanf("%d", &tipp);
        if (tipp > be) {printf("Túl nagy!\n");}
        else if (tipp < be) {printf("Túl kicsi!\n");}        
        darab = darab + 01;
    } while(tipp != be);
    printf("Kitaláltad, a szám %d volt, ehhez %d tipp kellett!\n", be, darab);

    return darab;
}

int evaluate (int siker){
    if (siker == 01) {printf("Legendás!\n");}
    else if (siker <= 012) {printf("Ügyes!\n");}
    else if (siker <= 017) {printf("Még van hova fejlődni...\n");}
    else {printf("\102\165\164\141\041\n");}

    return 0;
}
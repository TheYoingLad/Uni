#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "seged.h"

void printMenu(int invalid){
    printf("\n\n");
    printf("#####~~~~~~~~~~~~~~~~~~~#####\n");
    printf("#  Spirál Mátrix Generátor  #\n");
    printf("#####~~~~~~~~~~~~~~~~~~~#####\n\n");
    printf("0 - HELP\n");
    printf("1 - Mátrix generálása\n");
    printf("2 - Mátrix mentése\n");
    printf("3 - Mátrix betöltése\n");
    printf("4 - Mátrix megjelenítése\n");
    printf("5 - Exit\n%s", invalid?("Helytelen bemenet!\n"):("\n"));
    printf(">> ");
}

void guide(){
    printf("\n\n");
    printf("0: használati utasítások megnyitása\n\n");
    printf("1: n x n méretű spirálmátrix generálása, d kezdeti iránnyal és r forgási iránnyal\n");
    printf("   n: 1-20 egész\n   d: b|f|j|l (balra|fel|jobbra|le)\n   r: w|c (óramutató irányú|ellentétes irányú)\n\n");
    printf("2: elmenti a megfelelő fomrátummal a betöltött mátrixot egy fájlba (fájl neve tükrözi a mátrix tulajdonságait)\n\n");
    printf("3: betölt egy négyzetes mátrixot egy megfelelő formátumú t fájlból\n");
    printf("   t: fájl neve (aktuális könyvtárban) vagy elérési útja\n");
    printf("   formátum: egész számok az elemek, ';' az elemek között, sortörés a sorok közt\n\n");
    printf("4: megjeleníti a képernyőn a betöltött mátrixot\n\n");
    printf("5: kilépés a programból");
    cont();
}

void generate(spiral_t *sp){
    int n;
    char d, r;
    int invalid;
    char be[LENGTH];
    printf("\n");
    do{
        invalid = 1;
        printf("méret (1-20): ");
        fgets(be, LENGTH, stdin);
        if(atoi(be) > 0 && atoi(be) <= 20 && isdigit(be[0]) && (strlen(be) == 2 || (strlen(be) == 3 && isdigit(be[1])))){
            n = atoi(be);
            invalid = 0;
        } else printf("helyetelen bemenet\n\n");
    } while(invalid);
    printf("\n");
    do{
        invalid = 1;
        printf("kezdeti irány (b|f|j|l): ");
        fgets(be, LENGTH, stdin);
        if(strlen(be) == 2 && (be[0] == 'b' || be[0] == 'f' || be[0] == 'j' || be[0] == 'l')){
            d = be[0];
            invalid = 0;
        } else printf("helyetelen bemenet\n\n");
    } while(invalid);
    printf("\n");
    do{
        invalid = 1;
        printf("forgási irány (w|c): ");
        fgets(be, LENGTH, stdin);
        if(strlen(be) == 2 && (be[0] == 'w' || be[0] == 'c')){
            r = be[0];
            invalid = 0;
        } else printf("helyetelen bemenet\n\n");
    } while(invalid);
    int **m = spiralStart(n, d, r);
    if(!m) {cont(); return;} //sikertelen memóriafoglalás esetén kilép
    freeMatrix(sp);
    sp->size = n;
    sp->dir = d;
    sp->rot = r;
    sp->matrix = m;
    printf("\n%d x %d méretű, %c kezdeti irányú és %c forgású spirálmátrix generálva", n, n, toupper(d), r);
    cont();
}

void save(spiral_t *sp){
    if(sp->size == 0){
        printf("\nnincs betöltött mátrix");
        cont();
        return;
    }
    FILE *fp;
    char name[11];
    if(sp->dir != 0) getname(name, sp); //ha az irány 0, akkor fájlból belvasott mátrix van betöltve
    else getsize(name, sp);
    fp = fopen(name, "w");
    int i, j;
    for(i = 0; i < sp->size; i++){
        for(j = 0; j < sp->size; j++){
            fprintf(fp, "%d;", sp->matrix[i][j]);
        }
        fprintf(fp, "\n");
    }
    fclose(fp);
    printf("\na betöltött mátrix '%s' állományba sikeresen elmentve", name);
    cont();
}

void load(spiral_t *sp){
    char name[LENGTH];
    FILE *fp;
    printf("\n");
    do{
        printf("fájl neve: ");
        fgets(name, LENGTH, stdin);
        removeNL(name);
        fp = fopen(name, "r");
        if (!fp) printf("%s fájl nem létezik\n\n", name);
    } while(!fp);
    char sor[LENGTH];
    fgets(sor, LENGTH, fp);
    char *next = strtok(sor, ";");
    int n = -1;
    while(next){
        if(next[0] != 13) n++; //beolvasandó mátrix méretének meghatározása
        next = strtok(NULL, ";");
    }
    rewind(fp);
    int **m = allocate(n);
    if(!m) {cont(); return;} //sikertelen memóriafoglalás esetén kilép
    freeMatrix(sp);
    int i, j;
    for(i = 0; i < n; i++){
        fgets(sor, LENGTH, fp);
        next = strtok(sor, ";");
        for(j = 0; j < n; j++){
            m[i][j] = atoi(next);
            next = strtok(NULL, ";");
        }
    }
    sp->size = n;
    sp->matrix = m;
    fclose(fp);
    printf("\n'%s' állományból %d méretű mátrix sikeresen betöltve", name, n);
    cont();
}

void printMatrix(spiral_t *sp){
    if(sp->size == 0){
        printf("\nnincs betöltött mátrix");
        cont();
        return;
    }
    printf("\n");
    int max;
    if(sp->dir == 0) max = getmax(sp);
    else max = sp->size * sp->size;
    int len = 0;
    while(max != 0){
        max /= 10;
        len++; //spirálban előforduló legnagyobb szám számjegyeinek száma (lg nélkül)
    }
    printf("jelenleg betöltött mátrix:\n");
    if(sp->dir == 0) printf("típus: külső\nméret: %d\n", sp->size);
    else printf("típus: spirál\nméret: %d\nkezdeti irány: %c\nforgási irány: %c\n", sp->size, toupper(sp->dir), sp->rot);
    int i, j;
    for(i = 0; i < sp->size; i++){
        printf("\n");
        for(j = 0; j < sp->size; j++){
            printf("%*d ", len, sp->matrix[i][j]);
        }
    }
    cont();
}

void freeMatrix(spiral_t *sp){
    if(sp->size == 0) return;
    int i;
    for (i = 0; i < sp->size; i++) free(sp->matrix[i]);
    free(sp->matrix);
    sp->size = 0;
    sp->dir = 0;
    sp->rot = 0;
}
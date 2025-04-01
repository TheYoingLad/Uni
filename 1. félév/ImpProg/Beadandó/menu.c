#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "seged.h"

int main(){
    int opt;
    int invalid = 0;
    spiral_t *sp = malloc(sizeof(spiral_t));
    sp->size = 0; //ha a méret 0, akkor nincs betöltött mátrix
    while (1){
        opt = -1;
        char be[LENGTH];
        printMenu(invalid);
        fgets(be, LENGTH, stdin);
        if (strlen(be) == 2) opt = be[0];
        switch (opt){
            case 0 + '0':
                guide();
                invalid = 0;
                break;
            case 1 + '0':
                generate(sp);
                invalid = 0;
                break;
            case 2 + '0':
                save(sp);
                invalid = 0;
                break;
            case 3 + '0':
                load(sp);
                invalid = 0;
                break;
            case 4 + '0':
                printMatrix(sp);
                invalid = 0;
                break;
            case 5 + '0':
                freeMatrix(sp);
                free(sp);
                printf("\nViszlát!\n\n");
                return 0;
            default:
                invalid = 1;
        }
    }
}
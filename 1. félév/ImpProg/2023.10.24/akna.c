#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#define RESET       "\x1B[0m"
#define FGBLACK     "\x1B[30m"
#define FGRED       "\x1B[31m"
#define FGGREEN     "\x1B[32m"
#define FGCYAN      "\x1B[36m"
#define BGRED       "\x1B[41m"
#define BGGREEN     "\x1B[42m"
#define BGYELLOW    "\x1B[43m"
#define BGWHITE     "\x1B[107m"

int rows = 10;
int cols = 10;
void initTable(int, int[rows][cols]);
void drawTable(int[rows][cols], int[rows][cols]);
int getNeighbour(int, int, int[8]);
void toPos(int, int[2]);
void countNeighbourMines(int[rows][cols]);
void play(int, int[rows][cols], int[rows][cols]);
void reveal(int, int, int[rows][cols], int[rows][cols]);
void drawTableDone(int[rows][cols]);

int main(int argn, char *args[]){
    printf(RESET);
    if(argn < 2){
        printf(BGRED FGBLACK "Túl kevés argumentum!\n" RESET);
        return 0;
    }
    
    int n = atoi(args[1]);
    if(n > ((rows*cols)/3)){
        printf(BGRED FGBLACK "Túl sok akna!\n" RESET);
        return 0;
    }
    if(n < 3){
        printf(BGRED FGBLACK "Túl kevés akna!\n" RESET);
        return 0;
    }

    srand(time(NULL));
    
    int table[rows][cols];
    int showntable[rows][cols];
    
    initTable(n, table);
    initTable(0, showntable);
    countNeighbourMines(table);
    printf("\e[1;1H\e[2J" FGCYAN "Welcome to MineCweeper!\n\n\n" RESET);
    drawTable(table, showntable);
    play(n, table, showntable);
    //implement bomb markking w capital-normal letters
    //implement colored tiles w https://stackoverflow.com/questions/3585846/color-text-in-terminal-applications-in-unix
    printf(RESET);
}

void initTable(int n, int table[rows][cols]){
    int l, i, j;
    for(i = 0; i < rows; i++) for(j = 0; j < cols; j++) table[i][j] = 0;
    for(l = 0; l < n; l++){
        i = rand() % rows;
        j = rand() % cols;
        if(table[i][j] != -1) table[i][j] = -1;
        else l--;
    }
}

void drawTable(int table[rows][cols], int showntable[rows][cols]){
    //printf("\e[1;1H\e[2J");
    int i, j;
    printf(FGGREEN "\n   ");
    for(j = 0; j < cols; j++){
        if(j < 10) printf("  %d", j);
        else printf(" %d", j);
    }
    printf("\n\n" RESET);
    for(i = 0; i < rows; i++){
        printf(FGGREEN "%c  " RESET, 'A'+i);
        for(j = 0; j < cols; j++){
            if(showntable[i][j] == 1) printf("  %d", table[i][j]);
            else printf ("   ");
        }
        printf("\n");
    }
    printf("\n");
}

void drawTableDone(int table[rows][cols]){
    int i,j;
    printf(FGGREEN "\n   ");
    for(j = 0; j < cols; j++){
        if(j < 10) printf("  %d", j);
        else printf(" %d", j);
    }
    printf("\n\n");
    for(i = 0; i < rows; i++){
        printf(FGGREEN "%c  " RESET, 'A'+i);
        for(j = 0; j < cols; j++){            
            if(table[i][j] == -1) printf("  " BGRED FGBLACK "*" RESET);
            else printf("  %d", table[i][j]);            
        }
        printf("\n");
    }
    printf("\n");
}

int getNeighbour(int i, int j, int table[8]){
    if(i == 0 && j == 0)            {table[0] = 3; table[1] = 4; table[2] = 5; return 3;}
    if(i == 0 && j == cols-1)       {table[0] = 5; table[1] = 6; table[2] = 7; return 3;}
    if(i == rows-1 && j == cols-1)  {table[0] = 7; table[1] = 0; table[2] = 1; return 3;}
    if(i == rows-1 && j == 0)       {table[0] = 1; table[1] = 2; table[2] = 3; return 3;}
    if(i == 0)      {table[0] = 3; table[1] = 4; table[2] = 5; table[3] = 6; table[4] = 7; return 5;}
    if(j == cols-1) {table[0] = 0; table[1] = 1; table[2] = 5; table[3] = 6; table[4] = 7; return 5;}
    if(i == rows-1) {table[0] = 0; table[1] = 1; table[2] = 2; table[3] = 3; table[4] = 7; return 5;}
    if(j == 0)      {table[0] = 1; table[1] = 2; table[2] = 3; table[3] = 4; table[4] = 5; return 5;}
    table[0] = 0; table[1] = 1; table[2] = 2; table[3] = 3; table[4] = 4;  table[5] = 5;  table[6] = 6;  table[7] = 7; return 8;
}

void toPos(int be, int pos[2]){
    switch(be){
        case 0: {pos[0] = -1; pos[1] = -1; break;}
        case 1: {pos[0] = -1; pos[1] =  0; break;}
        case 2: {pos[0] = -1; pos[1] =  1; break;}
        case 3: {pos[0] =  0; pos[1] =  1; break;}
        case 4: {pos[0] =  1; pos[1] =  1; break;}
        case 5: {pos[0] =  1; pos[1] =  0; break;}
        case 6: {pos[0] =  1; pos[1] = -1; break;}
        case 7: {pos[0] =  0; pos[1] = -1; break;}
    }
}

void countNeighbourMines(int table[rows][cols]){
    int pos[2];
    int posTable[8];
    int i, j, n;
    for(i = 0; i < rows; i++){
        for(j = 0; j < cols; j++){
            n = getNeighbour(i, j, posTable);
            int l;
            for(l = 0; l < n; l++){
                toPos(posTable[l], pos);
                if(table[pos[0]+i][pos[1]+j] == -1 && table[i][j] != -1) table[i][j]++;
            }
        }
    }
}

void play(int n, int table[rows][cols], int showntable[rows][cols]){
    printf(BGYELLOW FGRED "%d mines remaining!\n\n" RESET, n);
    
    char be[255];
    printf("Tile: ");
    scanf ("%s", be);
    
    printf("\e[1;1H\e[2J");
    printf("Tile given: " BGWHITE FGBLACK "%s\n\n" RESET, be);
    if(strcmp(be, "altf4") == 0) return;

    int i = be[0] - 'A';
    be[0] = '0';
    int j = atoi(be);
    
    //printf("i = %d, j = %d, isShown: %d, value: %d\n", i, j, showntable[i][j], table[i][j]);
    if(i > rows-1 || i < 0) {
        printf(BGRED FGBLACK"Row out of range!\n" RESET);
        drawTable(table, showntable);
        play (n, table, showntable);
        return;
    } else if(j > cols-1 || j < 0) {
        printf(BGRED FGBLACK"Column out of range!\n" RESET);
        drawTable(table, showntable);
        play (n, table, showntable);
        return;
    } else if(showntable[i][j] == 1){
        printf(BGRED FGBLACK"Tile already revealed!\n" RESET);
        drawTable(table, showntable);
        play (n, table, showntable);
        return;
    } else if(table[i][j] == -1){
        printf(BGRED FGBLACK"KABOOM! You lost!\n" RESET);
        drawTableDone(table);
        return;
    } else printf("\n");
    
    reveal (i, j, table, showntable);
    drawTable(table, showntable);
    play(n, table, showntable);
    return;
}

void reveal(int i, int j, int table[rows][cols], int showntable[rows][cols]){
    showntable[i][j] = 1;
    int pos[2];
    int posTable[8];
    int n, l;
    if(table[i][j] == 0){
        n = getNeighbour(i, j, posTable);
        for(l = 0; l < n; l++){
            toPos(posTable[l], pos);
            if(showntable[pos[0]+i][pos[1]+j] == 0) reveal(pos[0]+i, pos[1]+j, table, showntable);
        }
    }
    return;
}
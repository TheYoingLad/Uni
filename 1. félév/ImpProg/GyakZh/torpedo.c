#include <stdio.h>
#include <stdlib.h>
#define WIDTH 10
#define HEIGHT 10

void init(int[HEIGHT][WIDTH], int[4]);
void printTable(int[HEIGHT][WIDTH]);
void submit(int[HEIGHT][WIDTH], int[4], char, int, int, char);
void validate(int[4]);

int main(){
    int tabla[HEIGHT][WIDTH];
    int hajok[4];
    printf("\nWelcome to torpedo!\nGeneratng map...\n\n\n");
    init(tabla, hajok);
    submit(tabla, hajok, 'A', 10, 3, '|');
    submit(tabla, hajok, 'I', 2, 3, '-');
    submit(tabla, hajok, 'I', 3, 3, '-');
    submit(tabla, hajok, 'F', 6, 5, '|');
    submit(tabla, hajok, 'F', 7, 5, '|');
    submit(tabla, hajok, 'A', 1, 2, '-');
    submit(tabla, hajok, 'I', 4, 4, '|');
    submit(tabla, hajok, 'C', 3, 4, '-');
    submit(tabla, hajok, 'G', 9, 3, '-');
    submit(tabla, hajok, 'B', 5, 3, '-');
    submit(tabla, hajok, 'I', 7, 3, '|');
    submit(tabla, hajok, 'J', 7, 2, '|');
    submit(tabla, hajok, 'J', 7, 1, '|');
    printTable(tabla);
    validate(hajok);
    return 0;
}

void init(int tabla[HEIGHT][WIDTH], int hajok[4]){
    int i, j;
    for(i = 0; i < HEIGHT; i++) for(j = 0; j < WIDTH; j++) tabla[i][j] = 0;
    for(i = 0; i < 4; i++) hajok[i] = 0;
}

void printTable(int tabla[HEIGHT][WIDTH]){
    int i, j;
    printf("\n    1  2  3  4  5  6  7  8  9  10\n");
    for(i = 0; i < HEIGHT; i++){
        printf("\n%c ", 'A' + i);
        for(j = 0; j < WIDTH; j++) printf("  %d", tabla[i][j]);
    }
    printf("\n\n");
}

void submit(int tabla[HEIGHT][WIDTH], int hajok[4], char rowchar, int col, int lenabs, char facechar){
    int row = rowchar - 'A';
    col--;
    int lenver, lenhor;
    if (facechar == '|'){
        lenhor = 1;
        lenver = lenabs;
    }else{
        lenhor = lenabs;
        lenver = 1;
    }
    if(lenabs < 2 || lenabs > 5){
        printf("length out of bounds\n\n");
        return;
    }
    if(row + lenver > 10 || col + lenhor > 10){
        printf("ship out of bounds\n\n");
        return;
    }
    int i, j;
    for(i = row - 1; i < row + lenver + 1; i++){
        for(j = col - 1; j < col + lenhor + 1; j++){
            if(i >= 0 && i <= HEIGHT - 1 && j >= 0 && j <= WIDTH - 1) {
                if(tabla[i][j] != 0){
                printf("ship position occupied\n\n");
                return;
                }
            }
        }
    }
    for(i = row; i < row + lenver; i++){
        for(j = col; j < col + lenhor; j++){
            tabla[i][j] = lenabs;
        }
    }
    hajok[lenabs-2]++;
    printf("ship with length %d facing %s has been added to the map at %c%d\n\n", lenabs, facechar == '|'?("vertically"):("horizontally"), rowchar, col + 1);
}

void validate(int hajok[4]){
    if(hajok[0] == 1 && hajok[1] == 2 && hajok[2] == 1 && hajok[3] == 1) printf("this map follows the rules\n");
    else printf("this map does not follow the rules\n");
}
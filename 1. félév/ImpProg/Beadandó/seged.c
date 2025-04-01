#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "seged.h"

void cont(){
    printf("\n\nNyomd meg az ENTER billentyűt a folytatáshoz...");
    getchar(); //vár ENTER-re
}

int **spiralStart(int n, char d, char r){
    int **m = allocate(n);
    if(m){ //sikeres-e a memóriafoglalás?
        coord_t c = initCoord(n, d, r);
        write(m, c, 1);
        spiral(m, n, d, r, c, 1, 1, 0);
    }
    return m;
}

int **allocate(int n){
    int **m = malloc(sizeof(int*)*n);
    if(!m){
        printf("\nsikertelen memóriafoglalás!");
        return NULL;
    }
    int i;
    for(i = 0; i < n; i++) {
        m[i] = malloc(sizeof(int)*n);
        if(!m[i]){
            printf("\nsikertelen memóriafoglalás!");
            return NULL;
        }
    }
    return m;
}

coord_t initCoord(int n, char d, char r){
    coord_t o, next1, next2;
    o.row = n/2;
    o.col = n/2;
    if(n % 2 == 1) return o;
    else{
        next1 = step(d);
        next2 = step(rotate(d, r));
        o = plus(o, plus(helper(next1), helper(next2)));
    }
    return o;
}

coord_t step(char d){
    coord_t c;
    switch(d){
        case 'b':
            c.row = 0;
            c.col = -1;
            break;
        case 'f':
            c.row = -1;
            c.col = 0;
            break;
        case 'j':
            c.row = 0;
            c.col = 1;
            break;
        case 'l':
            c.row = 1;
            c.col = 0;
            break;
    }
    return c;
}

coord_t plus(coord_t a, coord_t b){
    coord_t c;
    c.row = a.row + b.row;
    c.col = a.col + b.col;
    return c;
}

coord_t helper(coord_t a){
    coord_t c;
    c.row = (a.row == -1)?(0):(-a.row);
    c.col = (a.col == -1)?(0):(-a.col);
    return c;
}

void write(int **m, coord_t c, int val){
    m[c.row][c.col] = val;
}

void spiral(int **m, int n, char d, char r, coord_t c, int len, int val, int snd){
    int i = 0;
    while(i < len){
        c = plus(c, step(d));
        if(val == n * n) return;
        write(m, c, ++val);
        i++;
    }
    d = rotate(d, r);
    if(!snd) spiral(m, n, d, r, c, len, val, 1);
    else spiral(m, n, d, r, c, len + 1, val, 0);
}

char rotate(char d, char r){
    if(r == 'w'){
        switch(d){
            case 'b':
                return 'f';
            case 'f':
                return 'j';
            case 'j':
                return 'l';
            case 'l':
                return 'b';
        }
    } else{
        switch(d){
            case 'b':
                return 'l';
            case 'f':
                return 'b';
            case 'j':
                return 'f';
            case 'l':
                return 'j';
        }
    }
}

void getname(char *name, spiral_t *sp){
    int sizeIndent = 0;
    if(sp->size > 9) sizeIndent = 1;
    name[0] = '0' + (sp->size / 10);
    name[0 + sizeIndent] = '0' + (sp->size % 10);
    name[1 + sizeIndent] = toupper(sp->dir);
    name[2 + sizeIndent] = 'c';
    name[3 + sizeIndent] = '\0';
    if(sp->rot == 'c'){
        name[3 + sizeIndent] = 'c';
        name[4 + sizeIndent] = '\0';
    }
    strcat(name, "w.txt");
    //formátum: <méret><irány><forgás>.txt
}

void getsize(char *name, spiral_t *sp){
    int sizeIndent = 0;
    if(sp->size > 9) sizeIndent = 1;
    name[0] = '0' + (sp->size / 10);
    name[0 + sizeIndent] = '0' + (sp->size % 10);
    name[1 + sizeIndent] = '\0';
    strcat(name, "ext.txt");
    //formátum: <méret>ext.txt
}

void removeNL(char* str){
    if(!str) return;
    int len = strlen(str);
    if(str[len-1] == '\n') str[len-1] = '\0';
}

int getmax(spiral_t *sp){
    int max = sp->matrix[0][0];
    int i, j;
    for(i = 0; i < sp->size; i++) for(j = 0; j < sp->size; j++) if(sp->matrix[i][j] > max) max = sp->matrix[i][j];
    return max;
}
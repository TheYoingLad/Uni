#include <stdio.h>

int main(){
    FILE *fp;
    fp = fopen("even_numbers.txt", "a");
    int szamok[100];
    int i;
    for (i = 0; i < 100; i++) szamok[i] = i;
    printf("even_numbers.txt létrehozva!\n");
    for (i = 0; i < 100; i++) if (szamok[i] % 2 == 0) fprintf(fp, "%d ", szamok[i]);
    fprintf(fp, "\n");
    fclose(fp);
    return 0;
}
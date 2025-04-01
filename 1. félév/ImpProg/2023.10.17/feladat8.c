#include <stdio.h>

int main(){
    FILE *fp;
    fp = fopen("player.txt", "w");
    char nev[255];
    printf("name: ");
    scanf("%s", nev);
    fprintf(fp, "%s\n", nev);
    fclose(fp);
    return 0;
}
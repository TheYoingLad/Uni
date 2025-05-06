#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *args[]){
    //printf("%s, %s\n", args[0], args[1]);
    FILE *fp = fopen(args[1], "r");
    char szam[100];
    int sum = 0;
    while (fscanf(fp, "%s", szam) != EOF){
        printf("%d\n", atoi(szam));
        sum += atoi(szam);
    }
    printf("szumma: %d\n", sum);
    fclose(fp);
    return 0;
}
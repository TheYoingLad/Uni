#include <stdio.h>

int main(){
    FILE *fp = fopen("text.txt", "w+");
    fprintf(fp, "almafa\n");
    char[255] a;
    scanf("%s", a);
    fclose(fp);
    return 0;
}
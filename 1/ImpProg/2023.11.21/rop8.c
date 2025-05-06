#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int is_palindrom(char*, int);

int main() {
    char be[256];
    char c;
    int i = 0;
    while ((c = getchar()) != '\n' && i < 255) {
        be[i] = c;
        i++;
    }
    be[i] = '\0';
    int pal = is_palindrom(be, strlen(be));
    printf("%s\n", pal>=0?(pal?("palindrom"):("nem palindrom")):("sikertelen memoriafoglalas"));
    return 0;
}

int is_palindrom(char* str, int length){
    char* rev = malloc(sizeof(char)*length);
    if (rev == NULL) return -1;
    int i;
    for (i = 0; i < length; i++){
        rev[i] = str[length-i-1];
    }
    i = 0;
    while (i < length && rev[i] == str[i]) i++;
    free(rev);
    return i < length ? 0 : 1;
}
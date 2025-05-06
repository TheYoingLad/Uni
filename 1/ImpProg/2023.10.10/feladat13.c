#include <stdio.h>

int strlen (char[]);
int strcmp (char[], char[]);

int main() {
    char tomb1[] = "alma";
    char tomb2[] = "almafa";
    char tomb3[] = "bela";
    printf("tomb1 hossza: %d\n", strlen(tomb1));
    printf("tomb2 hossza: %d\n", strlen(tomb2));
    printf("tomb3 hossza: %d\n", strlen(tomb3));
    int comp12, comp13, comp23;
    comp12 = strcmp (tomb1, tomb2);
    comp13 = strcmp (tomb1, tomb3);
    comp23 = strcmp (tomb2, tomb3);
    printf("tomb 1, tomb 2: ");
    comp12 == 0?printf("%s = %s\n", tomb1, tomb2):(comp12 < 0?printf("%s hamarabb mint %s\n", tomb1, tomb2):printf("%s hamarabb mint %s\n", tomb2, tomb1));
    printf("tomb 1, tomb 3: ");
    comp13 == 0?printf("%s = %s\n", tomb1, tomb3):(comp13 < 0?printf("%s hamarabb mint %s\n", tomb1, tomb3):printf("%s hamarabb mint %s\n", tomb3, tomb1));
    printf("tomb 2, tomb 3: ");
    comp23 == 0?printf("%s = %s\n", tomb2, tomb3):(comp23 < 0?printf("%s hamarabb mint %s\n", tomb2, tomb3):printf("%s hamarabb mint %s\n", tomb3, tomb2));
    return 0;
}

int strlen (char be[]){
    int i = 0;
    while  (be[i++] != '\0') ;
    return --i;
}

int strcmp (char be1[], char be2[]){
    int i = 0;
    while (be1[i] != '\0'){
        if (be2[i] == '\0') return 1;
        if (be2[i] > be1[i]) return -1;
        if (be2[i] < be1[i]) return 1;
        i++;
    }
    if (be2[i] != '\0') return -1;
    return 0;
}
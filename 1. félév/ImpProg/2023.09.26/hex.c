#include <stdio.h>
#include <string.h>

int valid (char[], int);
long convert (char[], int);
long pow16 (int);

int main () {
    char be[10];
    int i, jo, hossz;
    long ki;

    do {
        printf("Kérem a hex számot! ");
        scanf("%s", be);
        hossz = strlen(be);
        jo = valid(be, hossz);
        if (jo == 0) printf("Helytelen bemenet!\n");
    } while (jo == 0);

    ki = convert(be, hossz);
    printf("A megadott szám decimális alakban: %li\n", ki);

    return 0;
}

int valid (char input[], int n) {
    int i;
    int output = 1;
    for (i = 0; i < n; i++) {
        if (!(('0' <=input[i] && '9' >= input[i]) || 
            ('a' <=input[i] && 'f' >= input[i]))) {output = 0;}
    }
    return output;
}

long pow16 (int exp) {
    int i;
    long output = 1;
    for (i = 0; i < exp; i++) {
        output *= 16;
    }
    return output;
}

long convert (char input[], int n) {
    int i;
    long output = 0;
    for (i = 0; i < n; i++) {
        if (input[i] < 58) {output += (input[i]-48) * pow16(n - i - 1);}
        else {output += (input[i]-87) * pow16(n - i - 1);}
    }
    return output;
}
#include <stdio.h>
#include <string.h>

int main(){
    FILE *f;
    f = fopen("./text.txt", "r+");
    char be[1100][32];
    int n = 0, i;
    do {
        fscanf(f, "%s", be[n]);
    } while (n++ < 1002);
    printf("5 vagy hosszabb:\n");
    for (i = 0; i < n; i++){
        if (strlen(be[i]) > 5) printf("%s ", be[i]);
    }    
    printf("\nx van benne:\n");
    for (i = 0; i < n; i++){
        if (strchr(be[i], 'x')) printf("%s ", be[i]);
    }
    printf("\nalma van benne:\n");
    for (i = 0; i < n; i++){
        if (strstr(be[i], "alma") != NULL) printf("%s ", be[i]);
    }
    printf("\nvan benne cica?\n");
    for (i = 0; i < n; i++){
        if (strcmp(be[i], "cica") == 0) printf("igen! ");
    }
    printf("\nszámjegy van benne:\n");
    for (i = 0; i < n; i++){
        if (strchr(be[i], '0')) printf ("%s ", be[i]);
        else if (strchr(be[i], '1')) printf ("%s ", be[i]);
        else if (strchr(be[i], '2')) printf ("%s ", be[i]);
        else if (strchr(be[i], '3')) printf ("%s ", be[i]);
        else if (strchr(be[i], '4')) printf ("%s ", be[i]);
        else if (strchr(be[i], '5')) printf ("%s ", be[i]);
        else if (strchr(be[i], '6')) printf ("%s ", be[i]);
        else if (strchr(be[i], '7')) printf ("%s ", be[i]);
        else if (strchr(be[i], '8')) printf ("%s ", be[i]);
        else if (strchr(be[i], '9')) printf ("%s ", be[i]);
    }
    printf("\n");
    return 0;
}
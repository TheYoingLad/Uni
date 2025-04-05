#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "seged.h"

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

int id;

int main()
{
    id = 0;

    Nyuszi a = {1, "alma", "ketto", 23};

    int f = open("alma.txt", O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
    write(f,&a,sizeof(a));
    close(f);

    /*FILE *fp = fopen("nyuszik.txt", "w"); //reset
    if (!fp)
    {
        printf("Fálj létrehozása sikertelen volt!\n");
        return -1;
    }
    fclose(fp);*/

    /*int invalid = 0;
    int opt;
    while (1)
    {
        opt = -1;
        char input[LENGTH];
        menu(invalid);
        fgets(input, LENGTH, stdin);
        if (strlen(input) == 2)
            opt = input[0];
        switch (opt)
        {
        case 0 + '0':
            printf("Viszlát!\n");
            return 0;
            break;
        case 1 + '0':
            jelentketzes();
            break;
        case 2 + '0':
            // modositas();
            break;
        case 3 + '0':
            // torles();
            break;printf("%d", sizeof(a));
        case 4 + '0':
            listazas();
            break;
        case 5 + '0':
            // pontozas();
            break;
        case 6 + '0':
            // eredmenyhirdetes();
            break;
        default:
            invalid = 1;
            break;
        }

        // módosítás
        // törlés
        // pontozás (random kapnak 1-10 valamennyit)
        // eredméynhirdetés
    }*/
}

void menu(int invalid)
{
    printf("\n\n");
    printf("<3<3<3<3<3<3<3<3<3<3<3<3<3<3<3\n");
    printf("|       Locsoló Király       |\n");
    printf("<3<3<3<3<3<3<3<3<3<3<3<3<3<3<3\n");
    printf("1 - Jelentkezés\n");
    printf("2 - Módosítás\n");
    printf("3 - Törlés\n");
    printf("4 - Listázás\n");
    printf("5 - Pontozás\n");
    printf("6 - Eredményhirdetés\n");
    printf("0 - Kilépés\n%s", invalid ? ("Helytelen bemenet!\n") : ("\n"));
    printf(">> ");
}

void jelentketzes()
{
    char nev[LENGTH];
    char vers[LENGTH];

    do
    {
        printf("Név: ");
        fgets(nev, LENGTH, stdin);
    } while (strlen(nev) == 1 || strchr(nev, ';') != NULL);
    nev[strlen(nev) - 1] = '\0'; // \n leszedés

    do
    {
        printf("Vers: ");
        fgets(vers, LENGTH, stdin);
    } while (strlen(vers) == 1 || strchr(vers, ';') != NULL);
    vers[strlen(vers) - 1] = '\0'; // \n leszedés

    FILE *fp = fopen("nyuszik.txt", "a");
    if (!fp)
    {
        printf("Fálj megnyitása sikertelen volt!\n");
        return;
    }
    fprintf(fp, "%d;%s;%s;0\n", id++, nev, vers);
    fclose(fp);

    varakozas();
}

void modositas()
{
}

void torles()
{
}

void listazas()
{
    FILE *reader = fopen("nyuszik.txt", "r");
    if (!reader)
    {
        printf("Fálj olvasása sikertelen volt!\n");
        return;
    }

    char data[LENGTH * 3];
    while (fgets(data, LENGTH * 3, reader) != NULL)
    {
        data[strlen(data) - 1] = '\0';

        char *token = strtok(data, ";");
        printf("Azonosító: %s\n", token);

        token = strtok(NULL, ";");
        printf("Név: %s\n", token);

        token = strtok(NULL, ";");
        printf("Vers: %s\n", token);

        token = strtok(NULL, ";");
        printf("Tojások: %s\n\n", token);
    }

    fclose(reader);

    varakozas();
}

void pontozas()
{
}

void eredmenyhirdetes()
{
}

void varakozas()
{
    char kuka[LENGTH];
    printf("(Nyomd meg az ENTER-t a folytatáshoz)");
    fgets(kuka, LENGTH, stdin);
}

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "seged.h"

int globalId;

int main()
{
    touch();
    getGlobalId();

    int invalid = 0;
    char opt;
    while (1)
    {
        opt = -1;
        char choice[3];

        menu(invalid);
        invalid = 0;

        fgets(choice, 3, stdin);
        if (strlen(choice) == 2)
            opt = choice[0];

        if (choice[1] != '\n' && choice[0] != '\n')
            clearStdin();

        switch (opt)
        {
        case 'q':
            printf("\nViszlát!\n");
            return 0;
            break;
        case 1 + '0':
            jelentketzes();
            break;
        case 2 + '0':
            modositas();
            break;
        case 3 + '0':
            torles();
            break;
        case 4 + '0':
            listazas();
            break;
        case 5 + '0':
            eredmenyhirdetes();
            break;
        default:
            invalid = 1;
            break;
        }
    }
}

void jelentketzes()
{
    char nev[MAX_NEV];
    char vers[MAX_VERS];

    readFromStdin(nev, MAX_NEV, "Név");
    readFromStdin(vers, MAX_VERS, "Vers");

    FILE *fp = fopen("nyuszik.txt", "a");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }
    fprintf(fp, "%d 0\n%s\n%s\n", globalId++, nev, vers);
    printf("Jelentkező sikeresen hozzáadva! Azonosító: %d\n", globalId - 1);

    fclose(fp);
    varakozas();
}

void modositas()
{
    FILE *fp = fopen("nyuszik.txt", "r");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }
    FILE *temp = fopen("nyuszik_tmp.txt", "w");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }

    int keresett;
    do
    {
        char input[MAX_NEV];
        printf("\nMódosítandó azonosítója: ");
        fgets(input, MAX_NEV, stdin);
        keresett = atoi(input);
        if (keresett < 1)
            printf("Érvénytelen azonosító!\n");
        if (input[strlen(input) - 1] != '\n')
            clearStdin();
    } while (keresett < 1);
    printf("\n");

    int volt = 0;
    while (!feof(fp))
    {
        int id, tojas;
        char nev[MAX_NEV];
        char vers[MAX_VERS];

        if (fscanf(fp, "%d %d ", &id, &tojas) == EOF)
            break;
        fgets(nev, MAX_NEV, fp);
        fgets(vers, MAX_VERS, fp);

        if (id == keresett)
        {
            int opt = 0;
            int invalid = 0;
            do
            {
                modosithato(invalid);
                invalid = 0;

                char choice[3];
                fgets(choice, 3, stdin);
                if (strlen(choice) == 2)
                    opt = atoi(choice);

                if (choice[1] != '\n' && choice[0] != '\n')
                    clearStdin();

                if (opt != 1 && opt != 2 && opt != 3)
                    invalid = 1;
            } while (invalid);

            int done;
            volt = 1;
            switch (opt)
            {
            case 1:
                readFromStdin(nev, MAX_NEV, "Új név");
                nev[strlen(nev)] = '\n';
                break;
            case 2:
                readFromStdin(vers, MAX_VERS, "Új vers");
                vers[strlen(vers)] = '\n';
                break;
            case 3:
                do
                {
                    char input[MAX_NEV];
                    done = 1;

                    printf("\nÚj tojások száma: ");
                    fgets(input, MAX_NEV, stdin);
                    tojas = atoi(input);

                    if (tojas < 0 || (tojas == 0 && input[0] != '0'))
                    {
                        printf("Érvénytelen darabszám!\n");
                        done = 0;
                    }
                    if (input[strlen(input) - 1] != '\n')
                        clearStdin();
                } while (!done);
                break;
            }
        }

        fprintf(temp, "%d %d\n%s%s", id, tojas, nev, vers);
    }
    printf("%s\n", volt ? "Sikeres módosítás!" : "A keresett azonosító nem található!");

    fclose(fp);
    fclose(temp);

    remove("nyuszik.txt");
    rename("nyuszik_tmp.txt", "nyuszik.txt");
    varakozas();
}

void torles()
{
    FILE *fp = fopen("nyuszik.txt", "r");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }
    FILE *temp = fopen("nyuszik_tmp.txt", "w");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }

    int keresett;
    do
    {
        char input[MAX_NEV];
        printf("\nTörlendő azonosítója: ");
        fgets(input, MAX_NEV, stdin);
        keresett = atoi(input);
        if (keresett < 1)
            printf("Érvénytelen azonosító!\n");
        if (input[strlen(input) - 1] != '\n')
            clearStdin();
    } while (keresett < 1);

    int volt = 0;
    while (!feof(fp))
    {
        int id, tojas;
        char nev[MAX_NEV];
        char vers[MAX_VERS];

        if (fscanf(fp, "%d %d ", &id, &tojas) == EOF)
            break;
        fgets(nev, MAX_NEV, fp);
        fgets(vers, MAX_VERS, fp);

        if (id != keresett)
            fprintf(temp, "%d %d\n%s%s", id, tojas, nev, vers);
        else
            volt = 1;
    }
    printf("%s\n", volt ? "Sikeres törlés!" : "A keresett azonosító nem található!");

    fclose(fp);
    fclose(temp);

    remove("nyuszik.txt");
    rename("nyuszik_tmp.txt", "nyuszik.txt");
    varakozas();
}

void listazas()
{
    FILE *fp = fopen("nyuszik.txt", "r");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }

    int volt = 0;
    while (!feof(fp))
    {
        int id, tojas;
        char nev[MAX_NEV];
        char vers[MAX_VERS];

        if (fscanf(fp, "%d %d ", &id, &tojas) == EOF)
            break;
        fgets(nev, MAX_NEV, fp);
        eol_remover(nev);
        fgets(vers, MAX_VERS, fp);
        eol_remover(vers);
        volt = 1;

        printf("\nAzonosító: %d\n", id);
        printf("Név: %s\n", nev);
        printf("Vers: %s\n", vers);
        printf("Tojások száma: %d\n", tojas);
    }
    printf("%s", volt ? "" : "Nem jelentkezett még senki a versenyre!\n");
    fclose(fp);
    varakozas();
}

void eredmenyhirdetes()
{
    FILE *fp = fopen("nyuszik.txt", "r");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }

    int maxId = 0;
    int maxTojas = -1;
    int id, tojas;
    while (fscanf(fp, "%d %d ", &id, &tojas) != EOF)
    {
        if (tojas > maxTojas)
        {
            maxId = id;
            maxTojas = tojas;
        }
        char buffer[MAX_VERS];
        fgets(buffer, MAX_VERS, fp);
    }

    if (maxId)
    {
        rewind(fp);
        while (!feof(fp))
        {
            int id, tojas;
            char nev[MAX_NEV];
            char vers[MAX_VERS];

            fscanf(fp, "%d %d ", &id, &tojas);
            fgets(nev, MAX_NEV, fp);
            fgets(vers, MAX_VERS, fp);

            if (id == maxId)
            {
                eol_remover(nev);
                eol_remover(vers);

                printf("Legtöbb tojást szerező adatai:\n");
                printf("\nAzonosító: %d\n", id);
                printf("Név: %s\n", nev);
                printf("Vers: %s\n", vers);
                printf("Tojások száma: %d\n", tojas);
                break;
            }
        }
    }
    else
        printf("Nem jelentkezett még senki a versenyre!\n");
    fclose(fp);
    varakozas();
}

// segéd függvények
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
    printf("5 - Eredményhirdetés\n");
    printf("q - Kilépés\n%s", invalid ? ("Helytelen bemenet!\n") : ("\n"));
    printf(">> ");
}

void varakozas()
{
    printf("\n(Nyomd meg az ENTER-t a folytatáshoz)");
    clearStdin();
}

void readFromStdin(char *s, int length, char *message)
{
    int done;
    do
    {
        done = 1;

        printf("\n%s: ", message);
        fgets(s, length, stdin);

        if (strlen(s) == 1)
        {
            printf("Az adat nem lehet üres!\n");
            done = 0;
        }
        /* if (done && strchr(s, ';') != NULL)
        {
            printf("Nem tartalmazhat ';' karaktert\n");
            done = 0;
        } */
        if (done && s[strlen(s) - 1] != '\n')
        {
            printf("Az adat túl hosszú!\n");
            done = 0;
            clearStdin(); // input buffer maradék részét elhagyom
        }
    } while (!done);
    eol_remover(s);
}

void clearStdin()
{
    char c = getchar();
    while (c != '\n')
        c = getchar();
}

void eol_remover(char *s)
{
    s[strlen(s) - 1] = '\0'; // \n leszedés a végéről
}

void modosithato(int invalid)
{
    printf("\nMódosítható adatok:\n");
    printf("1 - Név\n");
    printf("2 - Vers\n");
    printf("3 - Tojások száma\n%s", invalid ? ("Helytelen bemenet!\n") : ("\n"));
    printf(">> ");
}

void touch()
{
    FILE *fp = fopen("nyuszik.txt", "a");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }
    fclose(fp);
}

void getGlobalId()
{
    FILE *fp = fopen("nyuszik.txt", "r");
    if (!fp)
    {
        printf("Fájl megnyitása sikertelen volt!\n");
        return;
    }

    int a = 0, b = 0;
    while (fscanf(fp, "%d %d ", &a, &b) != EOF)
    {
        char buffer[MAX_VERS];
        fgets(buffer, MAX_VERS, fp);
    }
    globalId = a + 1;
    fclose(fp);
}
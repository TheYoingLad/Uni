#ifndef SEGED
#define SEGED

#define LENGTH 255

void menu(int invalid);
void jelentketzes();
void modositas();
void torles();
void listazas();
void pontozas();
void eredmenyhirdetes();
void varakozas();

typedef struct
{
    int id;
    char nev[LENGTH];
    char vers[LENGTH];
    int tojasok;
} Nyuszi;

#endif
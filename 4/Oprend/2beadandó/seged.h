#ifndef SEGED
#define SEGED

#define MAX_NEV 256
#define MAX_VERS 1024

void jelentketzes();
void modositas();
void torles();
void listazas();
void eredmenyhirdetes(int rovid);
void locsolas();
void menu(int invalid);
void varakozas();
void clearStdin();
void readFromStdin(char *s, int length, char *message);
void eol_remover(char *s);
void modosithato(int invalid);
void touch();
void getGlobalId();
void megerkezett(int signum);

#endif
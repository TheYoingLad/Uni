#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <wait.h>
#include <sys/sem.h>
#include <signal.h>
#include <sys/msg.h>
#include <time.h>

void handler(int signumber)
{
    printf("Kérdés megjött:\n");
}

struct uzenet
{
    long mtype;
    char mtext[1024];
};

int szemafor_letrehozas(key_t kulcs, int szemafor_ertek)
{
    int semid;
    if ((semid = semget(kulcs, 1, IPC_CREAT | S_IRUSR | S_IWUSR)) < 0)
        perror("semget");
    if (semctl(semid, 0, SETVAL, szemafor_ertek) < 0)
        perror("semctl");

    return semid;
}

void szemafor_muvelet(int semid, int op)
{
    struct sembuf muvelet;

    muvelet.sem_num = 0;
    muvelet.sem_op = op;
    muvelet.sem_flg = 0;

    if (semop(semid, &muvelet, 1) < 0)
        perror("semop");
}

void szemafor_torles(int semid)
{
    semctl(semid, 0, IPC_RMID);
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        perror("A paraméterek száma helytelen!\nElvárás:<típus> <érték>\n");
        return -1;
    }

    signal(SIGUSR1, handler);
    srand(time(NULL));

    // 1
    int pipefd[2];
    if (pipe(pipefd) == -1)
    {
        perror("Hiba a pipe nyitaskor!");
        exit(EXIT_FAILURE);
    }

    // 2
    key_t kulcs1 = ftok(argv[0], 1);
    int uzenetsor = msgget(kulcs1, 0600 | IPC_CREAT);
    if (uzenetsor < 0)
    {
        perror("msgget");
        return 1;
    }

    // 3
    key_t kulcs2 = ftok(argv[0], 2);
    int oszt_mem_id = shmget(kulcs2, 2048, IPC_CREAT | S_IRUSR | S_IWUSR);
    char *s = shmat(oszt_mem_id, NULL, 0);

    // 4
    key_t kulcs3 = ftok(argv[0], 3);
    int semid = szemafor_letrehozas(kulcs3, 0);

    pid_t child = fork();
    if (child == -1)
    {
        perror("Fork hiba");
        exit(EXIT_FAILURE);
    }

    if (child == 0) // gyerek
    {
        // 1
        close(pipefd[0]);

        kill(getppid(), SIGUSR1);

        char kerdes[100] = "Melyik fűrész akciós és éri meg ár/érték arányban?";
        write(pipefd[1], kerdes, sizeof(kerdes));

        close(pipefd[1]);

        // 2
        struct uzenet uz;
        int status;
        status = msgrcv(uzenetsor, &uz, 1024, 5, 0);
        
        char tipus[1024];
        int ar;
        if (status < 0)
        perror("msgsnd");
        else
        {
            char *token = strtok(uz.mtext, ";");
            strcpy(tipus, token);
            ar = atoi(strtok(NULL, ";"));

            printf("A bolt válasza: %s van %d forintért\n", tipus, ar);
        }

        // 3&4
        char buffer[2048];

        if (rand() % 2)
            snprintf(buffer, 2048, "Kérem a %s láncfűrészt %d forintért!", tipus, ar);
        else
            snprintf(buffer, 2048, "Sajnos nem fér bele, nincs elég moné :(");

        strcpy(s, buffer);
        szemafor_muvelet(semid, 1);
        shmdt(s);
    }
    else // szulo
    {
        // 1
        close(pipefd[1]);

        pause();

        char kerdes[100];
        read(pipefd[0], kerdes, sizeof(kerdes));

        printf("%s\n", kerdes);

        // 2
        struct uzenet uz;
        uz.mtype = 5;
        snprintf(uz.mtext, 1024, "%s;%d", argv[1], atoi(argv[2]));
        int status;

        status = msgsnd(uzenetsor, &uz, sizeof(uz.mtext), 0);
        if (status < 0)
            perror("msgsnd");

        wait(NULL);

        // 3 & 4
        szemafor_muvelet(semid, -1);
        printf("A vevő döntése: %s\n", s);
        szemafor_muvelet(semid, 1);

        shmdt(s);
        szemafor_torles(semid);
        shmctl(oszt_mem_id, IPC_RMID, NULL);
    }
    return 0;
}
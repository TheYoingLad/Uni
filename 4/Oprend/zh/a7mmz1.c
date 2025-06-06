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

#define MSGSIZE 1024

struct uzenet
{
    long mtype;
    char mtext[MSGSIZE];
};

void handler(int signumber, siginfo_t *info, void *nonused)
{
    char *msg = (char *)info->si_value.sival_ptr;
    printf("Munkás megérkezett, elkiáltja magát: %s\n", msg);
}

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
        perror("A paraméterek száma helytelen!\nElvárás:<betegség neve> <töménység>\n");
        return -1;
    }

    srand(time(NULL));

    // 1
    key_t kulcs1 = ftok(argv[0], 1);
    int uzenetsor = msgget(kulcs1, 0600 | IPC_CREAT);
    if (uzenetsor < 0)
    {
        perror("Hiba az üzenetsor nyitáskor");
        exit(EXIT_FAILURE);
    }

    char *signal_uzenet;
    struct sigaction sigact;
    sigact.sa_sigaction = handler;
    sigemptyset(&sigact.sa_mask);
    sigact.sa_flags = SA_SIGINFO;
    sigaction(SIGUSR1, &sigact, NULL);

    // 2
    int pipefd[2];
    if (pipe(pipefd) == -1)
    {
        perror("Hiba a pipe nyitáskor!");
        exit(EXIT_FAILURE);
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
        signal_uzenet = "Munkára, harca fel!";
        union sigval s_value_ptr;
        s_value_ptr.sival_ptr = signal_uzenet;

        sigqueue(getppid(), SIGUSR1, s_value_ptr);

        struct uzenet uz = {5, "Mely munka a legfontosabb?"};
        int status;

        status = msgsnd(uzenetsor, &uz, sizeof(uz.mtext), 0);
        if (status < 0)
        {
            perror("Hiba az üzenet küldése során");
            exit(EXIT_FAILURE);
        }

        // 2
        close(pipefd[1]);

        char feladat[MSGSIZE];
        read(pipefd[0], feladat, sizeof(feladat));

        printf("Válasz a főkertésztől: %s\n", feladat);

        // 3 & 4
        char buffer[MSGSIZE];

        if ((rand() % 10) < 4)
            snprintf(buffer, MSGSIZE, "Kérem a permetezés ismétlését egy héten belül");
        else
            snprintf(buffer, MSGSIZE, "Időben védekeztünk, nem szükésges ismételni!");

        strcpy(s, buffer);
        szemafor_muvelet(semid, 1);
        shmdt(s);
    }
    else // szulo
    {
        // 1
        pause();

        struct uzenet uz;
        int status;
        status = msgrcv(uzenetsor, &uz, MSGSIZE, 5, 0);

        if (status < 0)
        {
            perror("Hiba az üzenet érkezése során");
            exit(EXIT_FAILURE);
        }
        else
            printf("Kérdés a munkástól: %s\n", uz.mtext);

        // 2
        close(pipefd[0]);

        char feladat[MSGSIZE];
        snprintf(feladat, MSGSIZE, "Feladat: %s; Töménység: %s", argv[1], argv[2]);
        write(pipefd[1], feladat, sizeof(feladat));

        close(pipefd[1]);

        wait(NULL);

        // 3 & 4
        szemafor_muvelet(semid, -1);
        printf("Munkás javaslata: %s\n", s);
        szemafor_muvelet(semid, 1);

        shmdt(s);
        szemafor_torles(semid);
        shmctl(oszt_mem_id, IPC_RMID, NULL);
    }
    return 0;
}
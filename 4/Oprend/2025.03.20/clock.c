#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>

struct itimerval timer;

void handlersetitimer(int signalnumber)
{
  printf("tick\n");
}

int main(){
 signal(SIGALRM,handlersetitimer);

 alarm(1);

 struct sigaction sigact;
 sigemptyset(&sigact.sa_mask); //no blocked signals only the one, which arrives
 sigact.sa_handler=handlersetitimer;
 sigact.sa_flags=0; //no special behaviour
 sigaction(SIGALRM,&sigact,NULL); //an alarm signal is set

 timer.it_interval.tv_sec=1; /* it will be repeated after 3 seconds */
 timer.it_interval.tv_usec=0;
 timer.it_value.tv_sec=1;    /* remaining time till expiration (first interval) */
 timer.it_value.tv_usec=0;       

 setitimer(ITIMER_REAL,&timer,NULL ); //result = 0, if it is good  
  //when expires, a signal will be sent to the process, and it restarts
  //1. parameter ITIMER_REAL - real time, ITIMER_VIRTUAL (during process execution)
  //2. parameter timer 
  //3. old timer value
 while(1){
    ;
 }
 return 0;
}
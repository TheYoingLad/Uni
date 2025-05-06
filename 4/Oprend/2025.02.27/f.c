#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>  //fork
#include <sys/wait.h> //waitpid
#include <errno.h> 


int main()
{
   pid_t child = fork();
   
   if(child > 0){
      printf("I am a parent\n"); //\n szinte biztos, hogy kiirja a bufferbol a dolgokat
      fflush(stdout); //biztosan kiirja az adott streambol a dolgokat
   }
   else{
      printf("I am a child\n");
      fflush(stdout);
   } 
   
   return 0;
}
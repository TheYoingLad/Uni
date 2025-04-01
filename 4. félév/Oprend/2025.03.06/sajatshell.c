#include <stdio.h>

int main(){
    int status;

    while(1){
        char input[256];
        fgets(input, sizeof input);
        char* cmd = input[0];
        pid_t child = fork();

        if(child > 0){
            waitpid(child,&status,0); 
        } else{
            execv(cmd, input);
        }
    }
}
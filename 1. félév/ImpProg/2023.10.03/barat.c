#include <stdio.h>

int main(){
    int tomb[] = {12,220,142,10,20,0,564,284,5463,775,84};
    int size = sizeof(tomb)/sizeof(tomb[0])-1;
    int osztok[size];
    int i, j;
    int van = 0;
    for (i = 0; i < size; i++){
        int sum = 1;
        for(j = 2; j < tomb[i]; j++) if(tomb[i] % j == 0) sum += j;
        osztok[i] = sum;
    }
    i = 0;
    while(van == 0 && i < size){
        j = i + 1;
        while (van == 0 && j < size){
            if (osztok[i] == tomb[j] && osztok[j] == tomb[i]) van = 1;
            j++;
        }
        i++;
    }
    //for (i = 0; i < size; i++) printf("%d\n", osztok[i]);
    printf("%s\n", van==0?("Nincs barat"):("Van barat"));
    return 0;
}
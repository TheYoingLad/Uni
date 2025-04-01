#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "person.h"

int main(){
    person_t p1;
    char newname[] = "Janos";
    int len = strlen(newname);
    int i = 0;
    while(i < len){
        p1.name[i] = newname[i];
        i++;
    }
    p1.name[i] = '\0';
    p1.age = malloc(sizeof(int));
    *p1.age = 20;
    person_t p2;
    i = 0;
    while(i < len){
        p2.name[i] = p1.name[i];
        i++;
    }
    p2.name[i] = '\0';
    p2.age = malloc(sizeof(int));
    *p2.age = *p1.age;
    printf("1: név: %s, életkor: %d\n2: név: %s, életkor: %d\n", p1.name, *p1.age, p2.name, *p2.age);
    *p1.age = 50;
    printf("\n");
    printf("1: név: %s, életkor: %d\n2: név: %s, életkor: %d\n", p1.name, *p1.age, p2.name, *p2.age);
    return 0;
}
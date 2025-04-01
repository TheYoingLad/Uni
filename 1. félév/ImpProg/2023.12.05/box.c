#include <stdio.h>
#include <stdlib.h>
#include "box.h"

box_t* top;

int main(){
    top = initialize();
    printf("%s\n", isEmpty()?("empty"):("not empty"));
    push(5);
    push(10);
    printf("%s\n", isEmpty()?("empty"):("not empty"));
    printf("legfelso elem sulya: %d\n", peek(top));
    pop();
    printf("legfelso elem sulya: %d\n", peek(top));
    copyTop();
    printf("legfelso elem sulya: %d\n", peek(top));
    push(top->weight);
    printf("legfelso elem sulya: %d\n", peek(top));
    return 0;
}

box_t* initialize(){
    return NULL;
}

int isEmpty(){
    if (top) return 0;
    else return 1;
}

int peek(box_t* stack){
    if(stack) return stack->weight;
    else exit(1);
}

void push(int m){
    if(top){
        box_t* ujdoboz = malloc(sizeof(box_t));
        ujdoboz->weight = m;
        ujdoboz->next = top;
        top = ujdoboz;
    } else {
        top = malloc(sizeof(box_t));
        top->weight = m;
        top->next = NULL;
    }
}

void pop(){
    if(top){
        box_t* temp = top;
        top = top->next;
        free(temp);
    } else exit (1);
}

void copyTop(){
    box_t* felso = malloc(sizeof(box_t));
    felso->weight = top->weight;
    felso->next = top;
    top = felso;
}
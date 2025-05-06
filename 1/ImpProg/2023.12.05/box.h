#ifndef BOX
#define BOX
typedef struct box{
    int weight;
    struct box* next;
} box_t;
box_t* initialize();
int isEmpty();
int peek(box_t*);
void push(int);
void pop();
void copyTop();
#endif
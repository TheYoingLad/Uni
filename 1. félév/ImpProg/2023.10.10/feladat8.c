#include <stdio.h>

double avg (int*, int*);

int main() {
    int tomb[] = {1,2,3,4,5,6,7,8,9,10};
    double a = avg (tomb, tomb+10);
    printf("%.4lf\n", a);
    return 0;
}

double avg (int *eleje, int *vege){
    int sum = 0, i = 0;
    while (eleje+i != vege) {
        sum += *(eleje+i);
        i++;
    }
    double out = (double) sum / i;
    return out;
}
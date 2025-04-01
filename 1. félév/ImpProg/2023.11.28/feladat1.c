#include <stdio.h>

int main(){
    struct Student1{
        int az;
        double avg;
        int kor;
    };
    struct Student2{
        int az;
        int kor;
        double avg;
    };
    struct Student3{
        int kor;
        int az;
        double avg;
    };
    struct Student4{
        int kor;
        double avg;
        int az;
    };
    struct Student5{
        double avg;
        int kor;
        int az;
    };
    struct Student6{
        double avg;
        int az;
        int kor;
    };
    printf("student1: %d\n", sizeof(struct Student1));
    printf("student2: %d\n", sizeof(struct Student2));
    printf("student3: %d\n", sizeof(struct Student3));
    printf("student4: %d\n", sizeof(struct Student4));
    printf("student5: %d\n", sizeof(struct Student5));
    printf("student6: %d\n", sizeof(struct Student6));
    return 0;
}
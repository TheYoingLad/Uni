#include <stdio.h>

int main() {
    int *p;
    p = p;
    char *cp;
    double *dp;
    long *lp;
    char c = 'a';
    double d = 5.5;
    long l = 12L;
    int i = 1;
    printf("%p\n", p);
    p = &i;
    cp = &c;
    dp = &d;
    lp = &l;
    printf("%d, %p, %d\n", sizeof(p), p, *p);
    printf("%d, %p, %c\n", sizeof(cp), cp, *cp);
    printf("%d, %p, %lf\n", sizeof(dp), dp, *dp);
    printf("%d, %p, %li\n", sizeof(lp), lp, *lp);
    return 0;
}
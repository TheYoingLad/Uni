#include <math.h>
#include <stdio.h>

double ssz(double, double, double, double, double);
static const double pi = acos(-1);

int main(){
    double v11, v12, v21, v22, a;
    printf("1. vektor 1. komponens: ");
    scanf("%lf", &v11);
    printf("1. vektor 2. komponens: ");
    scanf("%lf", &v12);
    printf("2. vektor 1. komponens: ");
    scanf("%lf", &v21);
    printf("2. vektor 2. komponens: ");
    scanf("%lf", &v22);
    printf("a két vektor által bezárt szög fokban: ");
    scanf("%lf", &a);
    double s = ssz(v11, v12, v21, v22, a);
    printf("%.4f\n", s);
    return 0;
}

double ssz(double v11, double v12, double v21, double v22, double a){
    double l1 = hypot(v11, v12);
    double l2 = hypot(v21, v22);
    double arad = (a/180)*pi;
    return l1 * l2 * cos(arad);
}
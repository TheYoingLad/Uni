#include <math.h>
#include <stdio.h>

typedef struct{
    double x, y, z;
} Vec3D;

typedef double AVec3D[3];
//csigavonalban éretlmezhető, AVec3D-től kezdve => 3 elemű double tömb = AVec3D

void print_vector(Vec3D *v){
    printf("%f, %f, %f", v->x, v->y, v->z);
}

void vec_add(Vec3D *v1, Vec3D *v2, Vec3D *result){
    result->x = v1->x + v2->x;
    result->y = v1->y + v2->y;
    result->z = v1->z + v2->z;
}

Vec3D vec_add_ret(Vec3D *v1, Vec3D *v2){
    Vec3D result;
    result.x = v1->x + v2->x;
    result.y = v1->y + v2->y;
    result.z = v1->z + v2->z;
    return result;
}

void vec_multiply(Vec3D *v1, Vec3D *v2, Vec3D *result){
    result->x = v1->x * v2->x;
    result->y = v1->y * v2->y;
    result->z = v1->z * v2->z;
}

Vec3D vec_multiply_ret(Vec3D *v1, Vec3D *v2){
    Vec3D result;
    result.x = v1->x * v2->x;
    result.y = v1->y * v2->y;
    result.z = v1->z * v2->z;
    return result;
}

double vec_dot_product(Vec3D *v1, Vec3D *v2){
    return v1->x * v2->x + v1->y * v2->y + v1->z * v2->z;
}

int main(){
    Vec3D v1 = {1, 2, 3};
    Vec3D v2 = {4, 5, 6};

    vec_add(&v1, &v2, &v1);

    print_vector(&v1);
    return 0;
}
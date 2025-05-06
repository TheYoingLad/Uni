#include <cstdio>

struct Vec3D{
    double x, y, z;
};

void print_vector(Vec3D *v){
    printf("(%f, %f, %f)\n", v->x, v->y, v->z);
}

//overloading
void print_vector(Vec3D &v){
    printf("(%f, %f, %f)\n", v.x, v.y, v.z);
    //referenciát úgy kezeli, mintha ténylegesen a hivatkozott dolog lenne ott, implicit dereferál
    //álnévnek szokás nevezni
}

int main(){
    Vec3D v1 = {1, 2, 3};
    Vec3D v1_1 = {1, 2, 3};

    print_vector(&v1);
    print_vector(v1);               //implicit álnév
    
    const Vec3D v2 = {4, 5, 6};
    Vec3D *pv1 = &v1;
    pv1->x = 3;                     //ezen a pointeren kersztül változtatható v1 értéke

    //Vec3D *pv2 = &v2;             //ezt nem engedi a fordító, mert akkor nem teljesülne a const
    const Vec3D *pv2 = &v2;         //így jó, csak olvasni lehet a benti értéket
    double a = pv2->y;
    pv2 = &v1;                      //ezen a pointeren kersztül nem változtatható v1 értéke

    Vec3D *const cpv1 = &v1;        //pointer a konstans, értékeket lehet rajta keresztül átírni
    //cvpv1 = v1_1;                 //hiba

    const Vec3D *const cpv2 = &v2;  //a pointer érétke és a referált érték is konstans

    Vec3D &rv = v1;                 //álnév a v1-re
    rv = v2;                        // => v1 = v2
    //Vec3D &empty;                 //hiba, inicializálni kell a referenciát
    const Vec3D rv1 = v1;           //rajta keresztül nem lehet változtani az értékét
    //Vec3D & & rrv = ;             //értelmetlen
}
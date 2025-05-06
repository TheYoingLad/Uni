#ifndef FGV
#define FGV
    #define LENGTH 1000

    //típusok
    typedef struct {
        int size;
        char dir;
        char rot;
        int **matrix;
    } spiral_t;
    typedef struct {
        int row;
        int col;
    } coord_t;

    //kötelező
    void printMenu(int);
    void guide();
    void generate(spiral_t*);
    void save(spiral_t*);
    void load(spiral_t*);
    void printMatrix(spiral_t*);
    void freeMatrix(spiral_t*);

    //segéd
    void cont();
    int **spiralStart(int, char, char);
    int **allocate(int);
    coord_t initCoord(int, char, char);
    coord_t step(char);
    coord_t plus(coord_t, coord_t);
    coord_t helper(coord_t);
    void write(int**, coord_t, int);
    void spiral(int**, int, char, char, coord_t, int, int, int);
    char rotate(char, char);
    void getname(char*, spiral_t*);
    void getsize(char*, spiral_t*);
    void removeNL(char*);
    int getmax(spiral_t*);
#endif
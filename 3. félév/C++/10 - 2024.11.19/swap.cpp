#include <iostream>

struct NonAssignable
{
    NonAssignable &operator=(const NonAssignable &) = delete;
    void my_swap(NonAssignable &) {}
};

// fgv sablon
template <int count = 1, class T> // == template<typename T>
void swap(T &lhs, T &rhs)
{
    if (count % 2)
    {
        T tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }
}
void swap(int i, int j)
{
    std::cout << "ajaj\n";
}
// explicit specializáció
template<>
void swap(NonAssignable &lhs, NonAssignable &rhs)
{
    lhs.my_swap(rhs);
}

int main()
{
    int i = 123, j = 456;

    // sablon fgv
    swap<1, int>(i, j);

    // tipus dedukcio
    swap<>(i, j); // T = int; ha több típus paraméter van, akkor sem kell megadni az összeset

    swap(i, j); // ha PONTOS egyezés, akkor a nem sablon mindig előnyt élvez

    NonAssignable a, b;
    swap<4>(a, b);
}
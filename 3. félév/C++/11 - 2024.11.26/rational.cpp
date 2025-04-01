#include <utility>
#include <cassert>
#include <iostream>

int calc_gcd(int a, int b)
{
    while (b)
    {
        std::swap(a, b);
        b %= a;
    }
    return a;
}

struct RationalRuntime
{
    int nom, denom;

    RationalRuntime(int a, int b) : nom(a), denom(b)
    {
        assert(calc_gcd(a, b) == 1);
    }

    RationalRuntime operator*(const RationalRuntime &other) const
    {
        const int gcd = calc_gcd(nom * other.nom, denom * other.denom);
        return {nom * other.nom / gcd, denom * other.denom / gcd};
    }
};
void use_runtime()
{
    RationalRuntime r1{2, 3}, r2{3, 2};
    std::cout << (r1 * r2).nom << " / " << (r1 * r2).denom << '\n';
}

///////////////////////////////////////////////////////

template <int a, int b>
struct gcd
{
    static const int value = gcd<b, a % b>::value; // rekurzív eset
};
template <int a>
struct gcd<a, 0>
{
    static const int value = a; // végső eset
};

template <int n, int d>
struct RationalTemplate
{
    static_assert(gcd<n, d>::value == 1);

    static const int nom = n;
    static const int denom = d;
};

template <class R1, class R2>
struct mult // szorzás típus
{
    using common_gcd = gcd<R1::nom * R2::nom, R1::denom * R2::denom>;
    using type = RationalTemplate<R1::nom * R2::nom / common_gcd::value, R1::denom * R2::denom / common_gcd::value>; // szorzat eredményének típusa
};

void use_comptime(){
    using r1 = RationalTemplate<2, 3>;
    using r2 = RationalTemplate<10, 9>;
    using result = mult<r1, r2>;

    std::cout << result::type::nom << " / " << result::type::denom << '\n';
}

///////////////////////////////////////////////////////

int main()
{
    use_runtime();
    use_comptime();
}
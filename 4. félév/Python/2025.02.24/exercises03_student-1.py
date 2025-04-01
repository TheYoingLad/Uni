import time

# 1. Feladat
# Írd ki az összes számot 1-től 10-ig
def f1():
    for i in range(1,10):
        print(i)

# 2. Feladat
# Írd ki az összes páros számot 1-től 20-ig
def f2():
    for i in range(1,20):
        print(i)

# 3. Feladat
# Írd ki az 5-ös szorzótáblát
def f3():
    for i in range(5,46,5):
        print(i, end=" ")
    print()

# 4. Feladat
# Számold ki az összes szám összegét 1-től 100-ig
def f4():
    s = 0
    for i in range(101):
        s += i
    print(s)

# 5. Feladat
# Írd ki a Fibonacci sorozat első n elemét. Az n-t a felhasználótól kérjük be és ellenőrizzük, hogy megfelelő-e.
def f5():
    def fib(n: int):
        if n <= 2: return 1
        else: return fib(n-1) + fib(n-2)

    while True:
        try:
            n = int(input("n = "))
            if n <= 0: raise ValueError("n sould be a positive integer")
            print(f"the {n}. fibonacci  number = {fib(n)}")
            break
        except ValueError as e:
            print (e)

# 6. Feladat
# Írd ki az összes prímszámot n és m között
def f6():
    def primes(m: int):
        l = []
        for i in range(2, m+1):
            b = True
            for prime in l:
                if i % prime == 0: b = False
            else:
                if b: l.append(i)
        return l

    n = int(input("n = "))
    m = int(input("m = "))
    l = []
    for i in primes(m):
        if i >= n: l.append(i)
    print(f"primes metween {n} and {m} are: {l}")

# 7. Feladat
# Írd ki egy szám faktoriálisát
def f7():
    def fact(n: int):
        if n <= 1: return 1
        return n * fact(n-1)
    
    n = int(input("n = "))
    print(fact(n))

# 8. Feladat
# Fordíts meg egy adott számot 1234 -> 4321
def f8():
    def reverse_int(a: int):
        l = []
        while a >= 10:
            l.append(a % 10)
            a = a // 10
        l.append(a % 10)
        n = 0
        for digit in l:
            n = n * 10 + digit
        return n
    
    n = int(input("n = "))
    print(reverse_int(n))
    
# 9. Feladat
# Írj ki annyi információt egy számró amennyit tudsz (e.g.: páros, prímszám, tökéletes szám, számjegyek összege, osztók száma stb.)

# 10. Feladat
# Írd ki egy szám számjegyeinek összegét 1234 -> 10
def f10():
    def sum_digits(a: int):
        l = []
        while a >= 10:
            l.append(a % 10)
            a = a // 10
        l.append(a % 10)
        return sum(l)

# 11. Feladat
# Írd ki egy tömb elemeit két féle módon
def f11():
    def print_array(l: list):
        print(l)
        for i in l:
            print(i)

# 12. Feladat
# Találd meg a legnagyobb elemet egy tömbben
def f12():
    max_array = max

# 13. Feladat
# Fordíts meg egy tömböt anélkül, hogy az arr[::-1] szintaxist használnád
def f13():
    def reverse_array(l:list):
        l.reverse

# 14. Feladat
# Lapíts le egy 2D tömböt
def f14():
    def lapit(tomb:list):
        l = []
        for sor in tomb:
            for elem in sor:
                l.append(elem)
        return l

#------------------------------------------------------------------------------------------------------------------------

# 15. Feladat
# Írj egy dekorátort, amely megszámolja, hogy egy függvény hányszor lett meghívva
def f15():
    def szamlalo(func):
        def wrapper(*args, **kvargs):
            wrapper.calls += 1
            print(f"this was called {wrapper.calls} times")
            return func(*args, **kvargs)
        wrapper.calls = 0
        return wrapper

    @szamlalo
    def inc(a):
        return a + 1

    inc(inc(inc(inc(10))))

# 16. Feladat
# Írj egy dekorátort, amely méri, hogy egy függvény futása mennyi időt vett igénybe
def f16():
    def timer(func):
        def wrapper(*args, **kvars):
            start = float(time.time())
            result = func(*args, **kvars)
            print(f"This took {float(time.time()) - start}s to run")
            return result
        return wrapper

    @timer
    def longCalc(a):
        a = 2 ** 1000
    
    longCalc(2)

# 17. Feladat
# Írj egy dekorátort, amely egy függvény hívását valamilyen vizuális keretbe helyezi
def f17():
    def prettyPrint(func):
        def wrapper(*args, **kvargs):
            result = func(*args, **kvargs)
            print(f"The function {wrapper.name} was called with unnamed parameters {args} and named parameters {kvargs}, the result is {result}")
            return result
        wrapper.name = func.__name__
        return wrapper
    
    @prettyPrint
    def add(a: int, b: int) -> int:
        return a+b
    
    add(a=1, b=5)

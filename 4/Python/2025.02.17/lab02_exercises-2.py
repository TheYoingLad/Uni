# Basic Functions
# -----------------------------------------------------------------------------
#Írj egy függvényt ami egy számot vár paraméterül és visszaadja a szám négyzetét!
def square(a:int):
    return a**2

#Írj egy függvényt ami két számot vár paraméterül és kiírja a nagyobbat!
def greater(a:int, b:int) -> None:
    if(a>b): 
        print(a)
        return
    print(b)

#Írj egy függvényt ami egy számot vár paraméterül és visszaadja a szám abszolút értékét!(abs() kulcszó)
def absolute(a:int):
    return abs(a)

#Írj egy függvényt ami egy stringet vár paraméterül és megadja a string hosszát! (len() kulcszó)
def length(s:str):
    return len(s)

#Írj egy függvényt ami egy stringet vár paraméterül majd kiírja az utolsó karakterét!:
def last(s:str) -> None:
    print(s[len(s)-1])

#Írj egy függvényt, ami egy számot vár paraméterül és visszaadja a szám hárommal vett maradékát!
def three_remainder(a:int):
    return a%3

# Írj egy függvényt, ami egy számot vár paraméterül, és visszaadja, hogy a szám páros vagy páratlan! 
def even_odd(a:int):
    return a%2==0

# Írj egy függvényt, ami egy listát vár paraméterül, és visszaadja a lista legnagyobb elemét! (Használd a max() kulcsszót)
def greatest(l:list):
    return max(l)

# Írj egy függvényt, ami két stringet vár paraméterül és visszaadja, hogy az első string tartalmazza-e a másodikat! (Használd az "in" operátort)
def contains(a:str, b:str):
    return b in a

# Írj egy függvényt, ami egy listát vár paraméterül és visszaadja a lista elemeinek összegét! (Használd a sum() kulcsszót)
def szum(l : list):
    return sum(l)

# Írj egy függvényt, ami egy számot vár paraméterül és visszaadja, hogy a szám pozitív, negatív vagy nulla!
def neg_pos_zero(a: int):
    if a == 0: return "zero"
    if a > 0: return "positive"
    return "negative"

# Írj egy függvényt, ami egy stringet vár paraméterül és visszaadja a stringet nagybetűkkel! (Használd az upper() függvényt)
def yell(s : str):
    return s.upper()

# Írj egy függvényt, ami egy stringet vár paraméterül és visszaadja a stringet kisbetűkkel! (Használd a lower() függvényt)
def whisper(s : str):
    return s.lower()

# Írj egy függvényt, ami egy stringet vár paraméterül és visszaadja a stringet olyan módon, hogy az első betűje nagy a többi kicsi! (Használd a title() függvényt)
def cim(s : str):
    return s.title()

#Hibakezelés
# -----------------------------------------------------------------------------
# Feladat 1: Osztás nullával; idézd elő a ZeroDivisionError kivételt
try:
    alma = 1 / 0
except ZeroDivisionError as e:
    print(e)

# Feladat 2: Nem létező kulcs elérése egy szótárban; idézd elő a KeyError kivételt
try:
    a = {1:"alma", 2:"huhha"}
    b = a[3]
except KeyError as e:
    print(e)

# Feladat 3: Nem létező fájl megnyitása; idézd elő a FileNotFoundError kivételt
try:
    raise FileNotFoundError
except Exception as e:
    print(e)

# Feladat 4: Érvénytelen típusú művelet; idézd elő a TypeError kivételt
try:
    raise TypeError
except Exception as e:
    print(e)

# Feladat 5: Index hiba egy listában; idézd elő az IndexError kivételt
try:
    raise IndexError
except Exception as e:
    print(e)
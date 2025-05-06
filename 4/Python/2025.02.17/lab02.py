a = 10 
type(a) #int

fl = 10.3
fl = 10.
type(fl) #float

z = 1 + 3j
type(z) #complex

st = 'alma'
st = "alma"
st[0] # a
type(st) #str = string

b = False
type(b) #bool

l = [1,2,3,4,5,6,7,8,9]
type(l) #list
l[2:4] #3,4
l[6:] #7,8,9
l[1::2] #2,4,6,8
l[3:5:-1] #[]
l[5:3:-1] #[6, 5] => itt visszafele kell indexelni
#l[mettől:meddig:lépésköz]
l = [1,2,True,"helo"]

tpl = (1,2,3)
type(tpl) #tuple
#hasonló az indexelés a listához

rg = range(10) #0..9
type(rg) #range
rg = range(0, 10, 2) #0,2,4,6,8

dct = {"name":"John", "age":30}
type(dct) #dict
dct.keys() #"name", "age"
dct.values() #"John", 30
dct["name"] #John

s = {1,1,1,1,1,2,2,2,2,3,3,3} #1,2,3
type(s) #set

l2 = [1,1,1,1,1,2,2,2,2,3,3,3]
s2 = set(l2) #list -> set castolás, sorrendet nem tartja meg

a,b,c = 1,True,"jaj"

a,b = 1,2
a,b = b,a #a=2, b=1

a,b,c = [1,2,3] #a=1, b=2, c=3

"jaj"*3 #jajjajjaj
"jaj"+"helo" #jajhelo

print("alma", "fa") #almafa

alma = "egy kis fiu"
alma.title() #Egy Kis Fiu

def func(a, b):
    return a + b

a = 10
b = 20
print(func(a,b))
print(f"The function {func.__name__} was called with arguments {a} and {b} and returned {func(a,b)}")

def func2(a: int, b: int) -> int: #:int és -> int elhagyható, itt a visszatérési érték csak javaslat
    return a + b

def func3() -> None: #void
    pass

def eval(f, a, b) -> None:
    print(f"The function {f.__name__} was called with arguments {a} and {b} and returned {func(a,b)}")

eval(func2, 1, 2)
eval(func, "alma", "fa")

def five():
    return 5

def add_five(a):
    return a + five()

print(add_five(10))

try:
    print(1/0)
except ZeroDivisionError:
    print(f"A division by zero error occured")
except Exception as e:
    print(f"An error occured: {e}")
finally:
    print("done")
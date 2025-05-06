def my_decorator(func):
    def wrapper(*args, **kvargs):
        print("This is printed before the function is called\n")
        result = func(*args, **kvargs)
        print("This is printed after the function is called\n")
        return result
    return wrapper

def say_hello(a):
    print(f"Hello {a}")

say_hello = my_decorator(say_hello)
say_hello("Benedek")

@my_decorator
def fun(a):
    print(f"The square root of {a} is {a ** (1/2)}")

fun(4)

@my_decorator
def say_random_stuff(a : int = 12, b : str = "whatever") -> str:
    print(a*b)

say_random_stuff(a = 10, b = "why? ")
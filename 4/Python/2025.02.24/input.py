user = input("Enther your username: ")
print(f"Welcome {user}")
print(type(user))

pwd = int(input("Enther your password: "))
print(f"Your password is: {pwd}")
print(type(pwd))

l = input("Random list: ".split())
fl = float(input("Floating point numebr: "))

while True:
    try:
        a = int(input("Provide a positive integer: "))
        if a <= 0: raise ValueError("The number is not positive")
        break
    except ValueError as e:
        print(e)
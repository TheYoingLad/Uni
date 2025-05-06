def shout(text):
    return text.upper()

print(shout("huaaaa"))
yell = shout
print(yell("huaaaa"))

def whisper(text):
    return text.lower()

print(whisper("JEHEHEHHAJSDASD"))

def tmp(func):
    return func("I am in Spain but silent S")

print(tmp(yell))
print(tmp(whisper))

#----------------------------------------------

def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_15 = create_adder(15)
add_20 = create_adder(20)

print(add_15(2))
print(add_20(3))

l=[]
for i in range(100):
    l.append(create_adder(i))

for func in l:
    print(func(0), end=" ")
print()
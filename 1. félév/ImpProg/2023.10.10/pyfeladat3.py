x = [300,10]
print(id(x))
x[0] += 1
print(id(x))
y = x
print(id(y))
a = 50
b = a
print(id(a))
print(id(b))
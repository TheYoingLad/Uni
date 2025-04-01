def csere (x):
    c = x[0]
    x[0] = x[1]
    x[1] = c

x = [10, 30]
print("a = " + str(x[0]))
print("b = " + str(x[1]))
csere (x)
print("a = " + str(x[0]))
print("b = " + str(x[1]))
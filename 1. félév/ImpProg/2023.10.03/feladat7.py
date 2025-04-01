lista = [123, 123123123, 123123, 1, 31, 445, 7, 214, 268, 234, 6]
min = lista[0]
max = lista[0]
for i in lista:
    if (i<min):
        min = i
    if(i>max):
        max = i
min2 = max
for i in lista:
    if (i<min2 and i>min):
        min2 = i
print(lista)
print(min2)
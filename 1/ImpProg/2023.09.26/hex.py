while (True):
    i = 0
    hex = input("hex: ")
    for x in (hex):
        a = (x in [chr(n) for n in range(48, 58)])
        b = (x in [chr(n) for n in range(97, 103)])
        if (a or b):
            i = i + 1
    if (i == len(hex)):
        break
    else:
        print ("invalid!")
i = len(hex) - 1
out = 0
for x in (hex):
    if (ord(x) in [n for n in range(48, 58)]):
        out = out + (ord(x)-48) * (16 ** i)
    else: 
        out = out + (ord(x)-87) * (16 ** i)
    i = i - 1
print ("decimal: " + str(out))
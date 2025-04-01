import random

tipp = 0
num = random.randint(1,100)
while num != tipp:
    tipp = int(input("szam: "))
    if(tipp > num):
        print("tul nagy")
    elif(tipp < num):
        print("tul kicsi")
print("eltalaltad")
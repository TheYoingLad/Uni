with open("2025.03.31/data.txt", "r") as f: #autómatikus close, olvasásra nyitja
    data = f.read() #egész file
    print(data)

with open("2025.03.31/data.txt", "r") as f:
    data = f.readline() #egy sor
    print(data)

with open("2025.03.31/data.txt", "r") as f:
    data = f.readlines() #soronként
    print(data)

with open("2025.03.31/data.txt", "r") as f:
    data = [i.rstrip() for i in f.readlines()] #soronként, trailing whitespace nélkül
    print(data)

with open("2025.03.31/f.txt", "w") as f: #írásra nyitja, törli a tartalmát kezdéskor
    f.write("helo\n") #
    f.writelines(["keto\n","harom\n"]) #összes sor

with open("2025.03.31/f.txt", "a") as f: #írásra nyitja, végére ír
    f.write("omg\n") #
    f.writelines(["so\n","many\n"]) #összes sor
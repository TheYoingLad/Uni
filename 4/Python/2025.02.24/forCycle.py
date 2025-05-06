for i in range(1,10,2):
    print(i, end=" ")
print()

for i in range(10):
    for j in range(10):
        print(f"{i}{j}", end=" ")
    print()
else:
    print("Loop complete")

for i in range(10):
    if i %2 == 2: continue
    print(i, end = " ")
print()
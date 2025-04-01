pow2 = [2 ** x for x in range(10)]

# Equivalent with this:
pow2_ = []
for x in range(10):
   pow2_.append(2 ** x)

# Output: [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
print(pow2)
print(pow2_)

odd = [x for x in range(20) if x % 2 == 1]

print(odd)

str = [x+y for x in ['Python ','C '] 
              for y in ['Language','Programming']]

print(str)
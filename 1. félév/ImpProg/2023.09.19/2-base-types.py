
# Only the memory is the limit...
print(123123123123123123123123123123123123123123123123 + 1)

szam = 1000000000
print(szam)

# Int literals
binary = 0b0101
octal  = 0o01234
hexa   = 0xabcd

# Base-types
print(12, type(12))     # int
print(2.5, type(2.5))    # float
print(2+3j, type(2+3j))   # complex
print("str", type("str"))  # string
print(True, type(True))   # bool

# Conversions
print(int(123.5) == 123)
print(float(123) == 123.0)
print(bool(0)    == False)
print(bool(3)    == True)

# Even literals are objects
print("Methods: ", dir(123))
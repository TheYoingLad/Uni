#expr = lambda params : conequence => kb 1 parancsos fgv def nélkül

printing_lambda = lambda x : print(x)
printing_lambda("this is a text input")

l2 = lambda x, y : print(f"{x} is not {y}")
l2(1, "alma")

check_even = lambda x : "even" if x % 2 == 0 else "odd"
n = 11
print(f"{n} is {check_even(n)}")

#                                          V-iterálható objektum
map_example = list(map(lambda x : x**2, range(10)))
print(map_example)

filter_example = list(filter(lambda x : x%2 == 0, range(10)))
print(filter_example)
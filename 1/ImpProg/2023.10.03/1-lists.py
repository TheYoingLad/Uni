# empty list
list1 = []

# list of integers
list2 = [1, 2, 3]

# list with mixed datatypes
list3 = [1, "Hello", 3.4]

# nested list
list4 = ["mouse", [8, 4, 6], ['a']]

#Indexing

# Output: 1
print(list3[0])

# Output: Hello
print(list3[1])

# Output: 3.4
print(list3[2])

# Negativ indexes

# Output: 3.4
print(list3[-1])

# Output: Hello
print(list3[-2])

# Nested indexing

# Output: o
print(list4[0][1])    

# Output: 6
print(list4[1][2])

# Slicing

arr5 = ['a','l','m','a','f','a']
# elements 3rd to 5th : ['m', 'a', 'f']
print(arr5[2:5])

# elements 3rd to end : ['a', 'f', 'a']
print(arr5[3:])

# elements except the last two : ['a', 'l', 'm', 'a']
print(arr5[:-2])

# elements beginning to end
print(arr5[:])

# length of a list
print(len(arr5))
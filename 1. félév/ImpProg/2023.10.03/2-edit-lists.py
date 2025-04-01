# mistake values
odd = [2, 4, 6, 8]

# change the 1st item    
odd[0] = 1            

# Output: [1, 4, 6, 8]
print(odd)

# change 2nd to 4th items
odd[1:4] = [3, 5, 7]  

# Output: [1, 3, 5, 7]
print(odd)

# Output: [1, 3, 5, 7, 9, 11, 13]
print(odd + [9, 11, 13])

# Output: ["re", "re", "re"]
print(["re"] * 3)

# Extending by assigning a list to an empty slice
odd[2:2] = [-1,-1,-1]
print(odd)

# Deleting by assigning an empty list to a slice
odd[2:5] = []
print(odd)

# Deleting with 'del' (works on slices, lists too)
del odd[2]
print(odd)
import copy

arr = [5, 7, 2, 8, 3.4,]
#append() - Add an element to the end of the list
arr.append(23)
print(arr)

#extend() - Add all elements of a list to the another list
arr.extend([-1, -2, -2])
print(arr)

#insert() - Insert an item at the defined index
arr.insert(3,100)
print(arr)

#remove() - Removes an item from the list
arr.remove(3.4)
print(arr)

#pop() - Removes and returns an element at the given index
print(arr.pop(6))
print(arr)

#index() - Returns the index of the first matched item
print(arr.index(-2))
#Error: print(arr.index(6))

#count() - Returns the count of number of items passed as an argument
print(arr.count(-2))

#sort() - Sort items in a list in ascending order
arr.sort()
print(arr)

#reverse() - Reverse the order of items in the list
arr.reverse()
print("arr:", arr)

arrr = [[1,2,3], [4,5,6]]

print("Real deep copy")
#deepcopy() - Returns a recusively deep copy of the list
arr1 = copy.deepcopy(arrr)
arr1[0][0] = "really deep!"
print("arr:", arrr)
print("arr1:", arr1)

print("Deep copy")
#copy() - Returns a copy of the list
arr2 = arrr.copy()
arr2[1] = "deep"
arr2[0][0] = "really deep?"
print("arr:", arrr)
print("arr2:", arr2)

print("Shallow copy")
# shallow copy - really just a new name
arr3 = arrr
arr3[0] = "shallow"
print("arr:", arrr)
print("arr3:", arr3)

arr4 = arrr[:]
arr4[0] = "test"

#clear() - Removes all items from the list
arr2.clear()
arr3.clear()
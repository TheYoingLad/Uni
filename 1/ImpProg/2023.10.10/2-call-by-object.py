def mod_var(x):
    print("Inside, ", x, " id:", id(x))
    x=42
    print("Inside, ", x, " id:", id(x))

def mod_list(list):
    print("Inside, ", list, " id:", id(list))
    list.append(100)
    print("Inside, ", list, " id:", id(list))
    list = [100,101,102]
    print("Inside, ", list, " id:", id(list))
    
my_list = [1,2,3]
my_var = 10

print("Before, ", my_var, " id:", id(my_var))
mod_var(my_var)
print("After, ", my_var, " id:", id(my_var))
print("========================================")
print("Before, ", my_list, " id:", id(my_list))
mod_list(my_list)
print("After, ", my_list, " id:", id(my_list))
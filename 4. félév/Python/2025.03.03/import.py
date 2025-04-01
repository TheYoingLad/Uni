from vehicle import Vehicle
from vehicle import Auto

a = Vehicle(make="Toyota", model="Corolla", year=2020)
print(a.return_speed())

f = Auto(make="Ford", model="transit", year=2020, doors=6)
print(f.return_speed())
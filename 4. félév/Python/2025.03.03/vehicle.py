class Vehicle:
    def __init__(self, make, model, year) -> None:
        self.make = make
        self.model = model
        self.year = year

    def return_speed(self):
        pass

    def __del__(self): #dtor
        print("destructor called")

class Auto(Vehicle):
    def __init__(self, make, model, year, doors) -> None:
        super().__init__(make, model, year)
        self.doors = doors

    def return_speed(self) -> int:
        return 100


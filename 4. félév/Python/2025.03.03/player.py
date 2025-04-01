class Player:
    def __init__(self, name : str, points : int, hp : int):
        if(points < 0): raise ValueError("Points must be non negative")
        if(hp < 0): raise ValueError("HP must be non negative")
        self.name = name
        self.points = points
        self.hp = hp
    
    def add_points(self, points : int):
        if(points < 0): raise ValueError("Points must be non negative")
        self.points += points
    
    def remove_points(self, points : int):
        if(points < 0): raise ValueError("Points must be non negative")
        self.points -= points

    def add_hp(self, hp : int):
        if(hp < 0): raise ValueError("HP must be non negative")
        self.hp += hp
    
    def remove_hp(self, hp : int):
        if(hp < 0): raise ValueError("HP must be non negative")
        self.hp -= hp
    
    def get_points(self):
        return self.points

    def get_hp(self):
        return self.hp

    def get_name(self):
        return self.name

    def is_dead(self):
        return self.hp <= 0

    def __str__(self):
        return f"Name: {self.name}, HP: {self.hp}"
    
    def __del__(self):
        print("player object has been deleted")
    
    def __eq__(self, other) -> bool:
        if(type(self) != type(other)): return False
        return self.hp == other.hp
        print(hi)

    @staticmethod
    def attack_other_player(p1, p2):
        p2.remove_hp(p1.get_points)

# - bónusz feladat, kezeld az esetlegesen adódó hibákat; pl: rossz típusú paraméterek, negatív pontszámok, stb.
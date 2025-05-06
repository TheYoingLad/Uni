# Készítsd el a "játékos" osztályt egy új file-ban, ami a következő metódusokkal rendelkezik:
# - __init__: inicializálja a játékos nevét, pontszámát és életét
# - add_points: adott pontszámot ad a játékoshoz
# - remove_points: adott pontszámot vesz el a játékostól
# - add_life: életet ad a játékoshoz
# - remove_life: életet vesz el a játékostól
# - get_score: visszaadja a játékos pontszámát
# - get_lives: visszaadja a játékos életét
# - get_name: visszaadja a játékos nevét
# - is_dead: visszaadja, hogy a játékos élete >= 0-e
# - __str__: visszaadja a játékos nevét és pontszámát
# - __del__: kiírja, hogy a játékos objektumot törölték
# - __eq__: összehasonlítja két játékos pontszámát
# - attack_other_player: egy statikus metódus ami két játékost kap paraméterül és meghívja az egyik játékos remove_life metódusát a másik játékos pontszáma alapján
# - bónusz feladat, kezeld az esetlegesen adódó hibákat; pl: rossz típusú paraméterek, negatív pontszámok, stb.

class Player:
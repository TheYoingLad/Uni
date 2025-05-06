from fastapi import FastAPI, Request
import uvicorn
from pydantic import BaseModel
from typing import Dict, Optional

# Hozz létre egy FastAPI webszervert ami valamilyen porton fut az uvicorn segítségével
app = FastAPI()

# A root első körben legyen egy üdvözlő oldal
@app.get("/")
def root():
    return {"message":"welcome"}

# Hozz létre egy pydantic BaseModel adatszerkezetet aminek három adattagja van: idő[string/date] hőmérséklet[int/float/str] páratartalom[int/float/str]
class Item(BaseModel):
    time: str
    temp: int
    hum: int

# Hozz létre egy szótárat, ami az előző pontban létrehozott objektumokat tárolja
items: Dict[int, Item] = {}

# Hozd létre az @app.post végpontot ami fogad egy mérést, majd eltárolja azt. Jelezz vissza a küldő félnek, hogy az adat sikeresen tárolásra került
@app.post("/store")
def store(request: Request):
    id = max(items.keys()) + 1
    items[id] = request.json()
    return {"message":"data recieved"}

# Hozd létre az @app.get("/measurements") végpontot, ami megjeleníti az eddig mért adatokat valamilyen formában
@app.get("/measurements")
def list_data():
    return items

# Írj egy python szkriptet ami periódkusan posztol egy random generált mérést a végpontra


# (Nem az óra anyaga, önáló kutatás) Csinálj egy (akár) html oldalt, ami valamilyen módon megjeleníti a mérés eredményeit

# Rakd át publikusra az IP címet, hogy lásd telefonról is az oldalt

if __name__ == "__main__":
    uvicorn.run(app=app, host="127.0.0.1", port="10000")
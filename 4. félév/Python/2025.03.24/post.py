from fastapi import FastAPI
from typing import Optional
import uvicorn

app = FastAPI()

@app.post("/adat")                                           
def uj_adat_fogadasa(adat: dict):
    return {"uzenet": "Az adat sikeresen fogadva!", "kapott_adat": adat}

#curl -X POST "http://127.0.0.1:10000/adat" -H "Content-Type: application/json" -d "{\"nev\":\"Benedek\"}"

if __name__ == "__main__":
    uvicorn.run(app=app, host="127.0.0.1", port=10000)
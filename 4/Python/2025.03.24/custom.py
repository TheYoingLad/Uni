from fastapi import FastAPI, HTTPException
import uvicorn


app = FastAPI()

@app.get("/adat/{adat_id}")
def adat_id(adat_id : int):
    if adat_id == 1:
        return {"adat_id":adat_id, "nev":chr(0x0D9E)}
    elif adat_id == 2:
        return {"adat_id":adat_id, "nev":"Benedek"}
    else:
        raise HTTPException(status_code=404, detail="Az adat nem található")
    
if __name__ == "__main__":
    uvicorn.run(app, host="127.0.0.1", port=10000)
from fastapi import FastAPI
from typing import Optional
import uvicorn


app = FastAPI()

@app.get("/")
def root():
    return {"message":"root"}


@app.get("/udvozlet")                                     
def udvozlet_nevvel(nev: Optional[str] = None, pw: Optional[str] = None):
    if nev and not pw:
        return {"uzenet": f"Üdvözöllek, {nev}!"}
    elif nev and pw:
        return {"uzenet": f"You have sucessfully logged in with: {nev} and {pw}!"}
    return {"uzenet": "Üdvözöllek, látogató!"}

#curl -X GET "http://127.0.0.1:10000/udvozlet"
#curl -X GET "http://127.0.0.1:10000/udvozlet?name=Benedek"
#curl -X GET "http://127.0.0.1:20000/udvozlet?name=Benedek&pw=123"


if __name__ == "__main__":
    uvicorn.run(app=app, host="127.0.0.1", port=10000) #decimal number between 0 and 65535
    #uvicorn.run(app, host="0.0.0.0", port=10000)
    #New-NetFirewallRule -DisplayName "Allow Uvicorn" -Direction Inbound -Action Allow -Protocol TCP -LocalPort 8000
    #sudo ufw allow 8000/tcp

from fastapi import FastAPI
from routers.routes import routers as routes_router
from schemas.schema import ShopName

app = FastAPI()
app.include_router(routes_router)

@app.get('/')
def route():
    return {'Wellcome in ': ShopName}    

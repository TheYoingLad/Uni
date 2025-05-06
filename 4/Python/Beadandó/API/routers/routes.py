from schemas.schema import User, Basket, Item
from data.filereader import get_all_users, get_basket_by_user_id, get_total_price_of_basket, get_user_by_id
from data.filehandler import add_basket, add_item_to_basket, add_user, delete_item, update_item
from fastapi.responses import JSONResponse
from fastapi import HTTPException, APIRouter
from pydantic import ValidationError
from typing import List, Dict, Any
from data.helper import generateNewId, getTags

# FastAPI dokumentáció szerint ha megadunk response_modelt és visszatérési típust, akkor a response_model kap prioritást, ezért a visszatérési érték elhagyható, ezért elhagyom
# Hibajelzésként mindenhol 422-es hibakódot használtam, hiszen a felhasználó által adott adatok formailag helyesek de tartalmilag nem megfelelőek

routers = APIRouter()

@routers.post('/adduser', response_model=User, status_code=201)
def adduser(user: User):
    try:
        add_user(user.model_dump())
    except ValueError as err:        
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(status_code=201, content=user.model_dump())


# Alapértelmezett esetben user id + 100 a basket id, de ha az foglalt akkor keres újat ami még nem
@routers.post('/addshoppingbag', response_model=str, status_code=201)
def addshoppingbag(userid: int):
    try:
        ids = getTags(l=get_all_users(), name="id")
        baskets: List[Dict[str, Any]] = []
        for id in ids:
            try:
                baskets.append(get_basket_by_user_id(id))
            except:
                pass
        forbiddenIds = getTags(l=baskets, name="id")
        add_basket(Basket(id=generateNewId(l=forbiddenIds, origin=100+userid), user_id=userid).model_dump())
    except ValidationError as err: # pl ha a userid negatív
        raise HTTPException(status_code=422, detail=f"{err.errors()[0]["msg"]}: {err.errors()[0]["loc"][0]}")
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(status_code=201, content="Sikeres kosár hozzárendelés.")


@routers.post('/additem', response_model=Basket, status_code=201)
def additem(userid: int, item: Item):
    try:
        add_item_to_basket(user_id=userid, item=item.model_dump())
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(status_code=201, content=get_basket_by_user_id(user_id=userid))


# ha nincs a kosárban ilyen Item akkor hozzáadja, ennek megfelelő kóddal tér vissza
@routers.put('/updateitem', response_model=Basket)
def updateitem(userid: int, itemid: int, updateItem: Item):
    try:
        l = update_item(user_id=userid, item_id=itemid, item=updateItem.model_dump())
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(status_code=l, content=get_basket_by_user_id(user_id=userid))


@routers.delete('/deleteitem', response_model=Basket)
def deleteitem(userid: int, itemid: int):
    try:
        delete_item(user_id=userid, item_id=itemid)
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(content=get_basket_by_user_id(user_id=userid))


@routers.get('/user', response_model=User)
def user(userid: int):
    try:
        user = get_user_by_id(userid)
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(content=user)

@routers.get('/users', response_model=List[User])
def users():
    try:
        users = get_all_users()
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(content=users)


@routers.get('/shoppingbag', response_model=List[Item])
def shoppingbag(userid: int):
    try:
        basket = get_basket_by_user_id(user_id=userid)
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(content=basket["items"])


@routers.get('/getusertotal', response_model=float)
def getusertotal(userid: int):
    try:
        total = get_total_price_of_basket(user_id=userid)
    except ValueError as err:
        raise HTTPException(status_code=422, detail=err.__str__())
    return JSONResponse(content=total)
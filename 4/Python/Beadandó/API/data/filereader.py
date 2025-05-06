import json
from typing import Dict, Any, List
from data.helper import getTags, getWithTag, hasTag
from numpy import dot

JSON_FILE_PATH = "data/data.json"

def load_json() -> Dict[str, Any]:
    with open(JSON_FILE_PATH, "r", encoding="utf-8") as file:
        return json.load(file)


def get_user_by_id(user_id: int) -> Dict[str, Any]:
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    
    data = load_json()
    users = data["Users"]
    if not hasTag(l=users, name="id", value=user_id):
        raise ValueError("There is no user with this id!")

    return getWithTag(l=users, name="id", value=user_id)

# A feladat megoldása szempontjából itt értelmesebb az egész kosárral visszatérni, és nem csak egy listával (ami az Itemek listája).
# Ezt azért is gondolom, mert a router.py megvalósításánál Basket-ekkel kell visszatérni, amit értelem szerűen ennek a fgv-nek kellene produkálnia.
#def get_basket_by_user_id(user_id: int) -> List[Dict[str, Any]]: 
def get_basket_by_user_id(user_id: int) -> Dict[str, Any]: 
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    
    data = load_json()
    baskets = data["Baskets"]
    if not hasTag(l=baskets, name="user_id", value=user_id):
        raise ValueError("This user does not have a basket!")

    return getWithTag(l=baskets, name="user_id", value=user_id)


def get_all_users() -> List[Dict[str, Any]]:
    userList: List[Dict[str, Any]] = []

    data = load_json()
    users = data["Users"]
    userIds = getTags(l=users, name="id")

    for userId in userIds:
        userList.append(get_user_by_id(userId))
    
    return userList


def get_total_price_of_basket(user_id: int) -> float:
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    
    basket = get_basket_by_user_id(user_id)
    prices = getTags(l=basket["items"], name="price")
    quantities = getTags(l=basket["items"], name="quantity")

    return dot(prices, quantities)
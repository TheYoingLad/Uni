import json
from typing import Dict, Any
from schemas.schema import User, Basket, Item
from pydantic import ValidationError
from data.helper import hasTag, getWithTag

JSON_FILE_PATH = "data/data.json"

def load_json() -> Dict[str, Any]:
    with open(JSON_FILE_PATH, "r", encoding="utf-8") as file:
        return json.load(file)


def save_json(data: Dict[str, Any]) -> None:
    with open(JSON_FILE_PATH, "w", encoding="utf-8") as file:
        json.dump(data, file, ensure_ascii=False, indent=4)


def add_user(user: Dict[str, Any]) -> None:
    try:
        userValidated = User.model_validate_json(json.dumps(user))
    except ValidationError as err:
        raise ValueError(f"{err.errors()[0]["msg"]}: {err.errors()[0]["loc"][0]}")
    
    data = load_json()
    users = data["Users"]
    if hasTag(l=users, name="id", value=userValidated.id):
        raise ValueError("User id must be unique!")
    
    users.append(userValidated.model_dump())
    save_json(data)
    

def add_basket(basket: Dict[str, Any]) -> None:
    try:
        basketValidated = Basket.model_validate_json(json.dumps(basket))
    except ValidationError as err:
        raise ValueError(f"{err.errors()[0]["msg"]}: {err.errors()[0]["loc"][0]}")
    
    data = load_json()
    users = data["Users"]
    baskets = data["Baskets"]
    if not hasTag(l=users, name="id", value=basketValidated.user_id):
        raise ValueError("There is no user with this id!")
    if hasTag(l=baskets, name="user_id", value=basketValidated.user_id):
        raise ValueError("This user already has a basket!")
    if hasTag(l=baskets, name="id", value=basketValidated.id):
        raise ValueError("Basket id must be unique!")
    
    baskets.append(basketValidated.model_dump())
    save_json(data)


def add_item_to_basket(user_id: int, item: Dict[str, Any]) -> None:
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    try:
        itemValidated = Item.model_validate_json(json.dumps(item))
    except ValidationError as err:
        raise ValueError(f"{err.errors()[0]["msg"]}: {err.errors()[0]["loc"][0]}")
    
    data = load_json()
    baskets = data["Baskets"]
    if not hasTag(l=baskets, name="user_id", value=user_id):
        raise ValueError("This user does not have a basket!")
    
    items = getWithTag(l=baskets, name="user_id", value=user_id)["items"]
    if hasTag(l=items, name="item_id", value=itemValidated.item_id):
        raise ValueError("Item id must be unique in the basket!")

    items.append(itemValidated.model_dump())
    save_json(data)


def update_item(user_id: int, item_id: int, item: Dict[str, Any]) -> int:
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    if item_id < 0:
        raise ValueError("Input should be greater than or equal to 0: item_id")
    try:
        itemValidated = Item.model_validate_json(json.dumps(item))
    except ValidationError as err:
        raise ValueError(f"{err.errors()[0]["msg"]}: {err.errors()[0]["loc"][0]}")
    
    data = load_json()
    baskets = data["Baskets"]
    if not hasTag(l=baskets, name="user_id", value=user_id):
        raise ValueError("This user does not have a basket!")
    
    l = 200
    items = getWithTag(l=baskets, name="user_id", value=user_id)["items"]
    if item_id != itemValidated.item_id and hasTag(l=items, name="item_id", value=itemValidated.item_id):
        raise ValueError("New item id is not unique!")
    if hasTag(l=items, name="item_id", value=item_id):
        oldItem = getWithTag(l=items, name="item_id", value=item_id)
        for key in oldItem.keys():
            oldItem[key] = item[key]
    else:
        items.append(itemValidated.model_dump())
        l = 201
    
    save_json(data)
    return l


def delete_item(user_id: int, item_id: int) -> None:
    if user_id < 0:
        raise ValueError("Input should be greater than or equal to 0: user_id")
    if item_id < 0:
        raise ValueError("Input should be greater than or equal to 0: item_id")
    
    data = load_json()
    baskets = data["Baskets"]
    if not hasTag(l=baskets, name="user_id", value=user_id):
        raise ValueError("This user does not have a basket!")
    
    items = getWithTag(l=baskets, name="user_id", value=user_id)["items"]
    if not hasTag(l=items, name="item_id", value=item_id):
        raise ValueError("There is no item in this basket with this id!")

    items.remove(getWithTag(l=items, name="item_id", value=item_id))
    save_json(data)
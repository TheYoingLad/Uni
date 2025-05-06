from pydantic import BaseModel, NonNegativeInt, EmailStr, PositiveInt, PositiveFloat
from typing import List
      
ShopName='Gyors és Pihenő Bolt' #fast api rest api :)
  
class User(BaseModel):
    id: NonNegativeInt
    name: str
    email: EmailStr
    
class Item(BaseModel):
    item_id: NonNegativeInt
    name: str
    brand: str
    price: PositiveFloat
    quantity: PositiveInt

class Basket(BaseModel):
    id: NonNegativeInt
    user_id: NonNegativeInt
    items: List[Item] = []
from pydantic import BaseModel
from typing import Dict, Optional

# Define the Item model
class Item(BaseModel):
    name: str
    description: Optional[str] = None
    price: float
    tax: Optional[float] = None

# Data structure to store items (dictionary-based)
items_db: Dict[str, Item] = {}

# Function to add a new item
def add_item(item_id: str, item: Item):
    items_db[item_id] = item
    print(f"Item '{item_id}' added successfully.")

# Function to update an item
def update_item(item_id: str, new_data: dict):
    if item_id in items_db:
        for key, value in new_data.items():
            setattr(items_db[item_id], key, value)
        print(f"Item '{item_id}' updated successfully.")
    else:
        print(f"Item '{item_id}' not found.")

# Function to delete an item
def delete_item(item_id: str):
    if item_id in items_db:
        del items_db[item_id]
        print(f"Item '{item_id}' deleted successfully.")
    else:
        print(f"Item '{item_id}' not found.")

# Function to list all items
def list_items():
    return items_db
print(list_items())
# Example usage
add_item("item1", Item(name="Laptop", description="Gaming laptop", price=1200.99, tax=100))
print(list_items())
add_item("item2", Item(name="Phone", description="Smartphone", price=799.99))
print(list_items())
update_item("item1", {"price": 1100.50, "description": "Updated gaming laptop"})
print(list_items())
delete_item("item2")

# Print all items
print(list_items())

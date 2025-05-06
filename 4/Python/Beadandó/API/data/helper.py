from typing import List, Dict, Any

# segéd fgv-ek a json kezeléshez

def hasTag(l: List[Dict[str, Any]], name: str, value: Any) -> bool:
    for dictionary in l:
        if dictionary[name] == value:
            return True
    return False

def getWithTag(l: List[Dict[str, Any]], name: str, value:Any) -> Dict[str, Any]:
    for dictionary in l:
        if dictionary[name] == value:
            return dictionary 
    return {}   

def getTags(l: List[Dict[str, Any]], name: str) -> List[Any]:
    tags: List[Any] = []
    for dictionary in l:
        tags.append(dictionary[name])
    return tags

def generateNewId(l: List[int], origin: int) -> int:
    i = origin
    while(l.__contains__(i)):
        i += 1
    return i
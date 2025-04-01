package data.structure;

import java.util.*;

public class MultiSet<E>{
    private HashMap<E,Integer> elemToCount = new HashMap<>();

    public MultiSet(E[] keys){
        for(E key:keys) add(key);
    }

    public int add(E key){
        elemToCount.put(key, elemToCount.getOrDefault(key, 0)+1);
        return elemToCount.get(key);
    }
    public int getCount(E key){
        return elemToCount.getOrDefault(key, 0);
    }
    public MultiSet<E> intersect(MultiSet<E> other){
        MultiSet<E> m = new MultiSet<>((E[])new Object[0]);
        for(E key:elemToCount.keySet()) if(other.elemToCount.containsKey(key)) m.elemToCount.put(key, Math.min(getCount(key), other.getCount(key)));
        return m;
    }
    public int size(){
        return elemToCount.size();
    }
}
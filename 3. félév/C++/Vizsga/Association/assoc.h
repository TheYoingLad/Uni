#ifndef ASSOC
#define ASSOC

#include <vector>
#include <functional>
#include <algorithm>
#include <set>

template<class K, class Cont, class T = typename Cont::value_type, class Comp = std::equal_to<K> >
class association_common{
    using iterator = typename Cont::iterator;
    using key_iterator = typename std::vector<K>::const_iterator;
    protected:
    std::vector<K> keys;
    std::vector<iterator> iters;
    Cont *cont;
    Comp comp;

    public:
    association_common(Cont& c){
        cont = &c;
    }

    bool has(const K& key) const{
        for (key_iterator current = keys.begin(); current != keys.end(); ++current){
            if(comp(key, *(current))) return true;
        }
        return false;
    }

    void associate(const K& key, iterator it){
        if(has(key)) return;
        keys.push_back(key);
        iters.push_back(it);
    }

    T find(const K& key) const{
        int i = 0;
        for (key_iterator current = keys.begin(); current != keys.end(); ++current){
            if(comp(key, *(current))) return *(iters[i]);
            ++i;
        }
        return T();
    }

};


template<class K, class Cont, class T = typename Cont::value_type, class Comp = std::equal_to<K> >
class association : public association_common<K, Cont, T, Comp>{
    using iterator = typename Cont::iterator;
    using key_iterator = typename std::vector<K>::const_iterator;
    using a = association_common<K, Cont, T, Comp>;
    public:
    association(Cont& c) : association_common<K, Cont, T, Comp>(c) {}

    T& operator[](const K& key){
        if(a::has(key)) {
            int i = 0;
            for (key_iterator current = a::keys.begin(); current != a::keys.end(); ++current){
                if(a::comp(key, *(current))) return *(a::iters[i]);
                ++i;
            }
        }
        
        T base;
        a::cont->push_back(base);
        iterator it = std::find(a::cont->begin(), a::cont->end(), base);
        a::associate(key, it);
        
        return (*it);
    }
};

template<class K, class T, class Comp>
class association<K, std::set<T, Comp> > : public association_common<K, std::set<T, Comp>>{
    using iterator = typename std::set<T, Comp>::iterator;
    using key_iterator = typename std::vector<K>::const_iterator;
    using a = association_common<K, std::set<T, Comp>>;
    public:
    association(std::set<T, Comp>& c) : association_common<K, std::set<T, Comp>>(c) {}

    const T& operator[](const K& key){
        if(a::has(key)) {
            int i = 0;
            for (key_iterator current = a::keys.begin(); current != a::keys.end(); ++current){
                if(a::comp(key, *(current))) return *(a::iters[i]);
                ++i;
            }
        }
        
        T base;
        a::cont->insert(base);
        iterator it = a::cont->find(base);
        a::associate(key, it);
        
        return (*it);
    }
};










#endif
#pragma once

#include <vector>
#include <algorithm>
#include <functional>

template <typename T, typename Cmp = std::less<T> >
class SortedList
{
    using data_t = std::vector<T>;
    using iterator = typename data_t::iterator;
    using const_iterator = typename const data_t::iterator;

    data_t data;

public:
    SortedList(){

    }

    void insert(const T& e)
    {
        iterator it = std::lower_bound(data.begin(), data.end(), e, Cmp()); //ez kihasználja, hogy az adat rendezett
        data.insert(it, e);

        /* 2. megoldás
        data.push_back(e);
        std::sort(data.begin(), data.end(), ?Cmp()?);
        */
    }

    int size() const
    {
        return data.size();
    }

    /* ha kell változtatni az elemet
    T& front()
    {
        return data.front();
    }*/
    const T& front() const
    {
        return data.front();
    }

    void remove(const T& e){
        iterator first = std::lower_bound(data.begin(), data.end(), e, Cmp());
        data.erase(first);

        /* összes ilyen elem törlése
        iterator last = first;
        while(last != data.end() && *last == e){
            last++;
        }
        data.erase(first, last);

        vagy

        std::pair<iterator, iterator> range = std::equal_range(data.begin(), data.end(), e, Cmp());
        data.erase(range.first, range.second);
        */
    }

    /* ha kell változtatni az elemet
    T& back()
    {
        return data.back();
    }*/
    const T& back() const{
        return data.back();
    }

    /* ha kell
    iterator begin(){
        return data.begin();
    }*/
    const_iterator begin() const{
        return data.begin();
    }
    
    /* ha kell
    iterator end() {
        return data.end();
    }*/
    const_iterator end() const{
        return data.end();
    }
};
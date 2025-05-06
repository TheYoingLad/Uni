#include <vector>
#include <algorithm>
#include <functional>

#pragma once

template<class T, class Cmp = std::less<T> >
class SortedList{
    using data_t = std::vector<T>;
    using iterator = typename data_t::iterator;
    using const_iterator = typename data_t::const_iterator;

    data_t data;

    public:
        SortedList(){}

        template<class Iterator>
        SortedList(Iterator begin, Iterator end){
            while(begin != end){
                insert(*begin);
                begin++;
            }
        }

        void insert(const T& a){
            iterator it = std::lower_bound(data.begin(), data.end(), a, Cmp());
            data.insert(it, a);
        }

        int size() const{
            return data.size();
        }

        const T& front() const{
            return data.front();
        }

        void remove(const T& a){
            iterator first = std::lower_bound(data.begin(), data.end(), a, Cmp());
            data.erase(first);
        }

        const T& back() const{
            return data.back();
        }

        const_iterator begin() const{
            return data.begin();
        }

        const_iterator end() const{
            return data.end();
        }
};
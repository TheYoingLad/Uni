#ifndef LISTVIEW_H
#define  LISTVIEW_H
#include <functional>
#include <list>
#include <vector>
#include <algorithm>

template <class T, class Cont = std::vector<typename std::list<T>::iterator> >
class array_view{
    protected:
    std::list<T>* list;
    Cont array;
    public:
    array_view(std::list<T>& l){
        list = &l; // a listát nem másolhatom le!
        for(typename std::list<T>::iterator it = l.begin(); it != l.end(); ++it){
            array.push_back(it);
        }
    }
    T& at(int idx){
        return *array[idx];
    }
    const T& at(int idx) const {
        return *array[idx];
    }
    T& operator[](int idx){
        return at(idx);
    }
    const T& operator[](int idx) const {
        return at(idx);
    }
};

template<class T, class Cont = std::vector<typename std::list<T>::iterator> >
class vector_view : public array_view<T,Cont>{
    public:
    vector_view(std::list<T>& l) : array_view<T, Cont>(l) {

    }
    void push_back(T t){
        this->list->push_back(t);
        this->array.push_back(--this->list->end());
    }
    /* my solution, ezzel is megadták az 5öst
    template<typename Comparator>
    void sort(Comparator comp){
        for(typename std::vector<typename std::list<T>::iterator>::iterator it = this->array.begin(); it != this->array.end() - 1; ++it){
            for(typename std::vector<typename std::list<T>::iterator>::iterator in = it; in != this->array.end(); ++in){
                if(!comp(**it, **in)){
                    typename std::list<T>::iterator temp = *it;
                    *it = *in;
                    *in = temp;
                }
            }
        }
    }
    void sort(){
        sort(std::less<T>());
    }
    */
    // the solution i wanted
    template <typename Compare>
    struct MyOrderWrapper{
        Compare comp;
        MyOrderWrapper(Compare& c) : comp(c){}
        bool operator()(typename std::list<T>::iterator lhs, typename std::list<T>::iterator rhs){ return comp(*lhs, *rhs); }
    };
    template <typename Compare>
    void sort(Compare Comp){
        MyOrderWrapper m(Comp);
        std::sort(this->array.begin(), this->array.end(), m);
    }
    void sort(){
        sort(std::less<T>());
    }
};
#endif

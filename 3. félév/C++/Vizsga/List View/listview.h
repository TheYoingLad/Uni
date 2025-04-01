#ifndef LIST_VIEW
#define LIST_VIEW

#include <vector>
#include <list>
#include <functional>
#include <algorithm>

template<class T, class Cont = std::vector<class std::list<T>::iterator> >
class array_view{
    using iterator = typename std::list<T>::iterator;
    
    protected:
        std::list<T> *list;
        Cont array;

    
    public:
        array_view(std::list<T>& l){
            list = &l;
            for(iterator it = l.begin(); it != l.end(); ++it){
                array.push_back(it);
            }
        }

        T& at(int i){
            return *array[i];
        }

        const T& at(int i) const{
            return *array[i];
        }

        T& operator[](int i){
            return at(i);
        }

        const T& operator[](int i) const{
            return at(i);
        }
};

template<class T, class Cont = std::vector<class std::list<T>::iterator> >
class vector_view : public array_view<T, Cont>{
    using iterator = typename std::list<T>::iterator;
    
    public:
        vector_view(std::list<T>& l) : array_view<T, Cont>(l){}

        void push_back(T a){
            this->list->push_back(a);
            this->array.push_back(--this->list->end());
        }

        template<class Comparator>
        void sort(Comparator comp){
            MyComp m(comp);
            std::sort(this->array.begin(), this->array.end(), m);
        }

        void sort(){
            sort(std::less<T>());
        }

    template<class Comparator>
    struct MyComp{
        Comparator comp;
        MyComp(Comparator c) : comp(c){}
        bool operator()(iterator it1, iterator it2){
            return comp((*it1), (*it2));
        }
    };
};

#endif
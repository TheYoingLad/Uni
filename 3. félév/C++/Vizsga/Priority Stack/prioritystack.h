#ifndef PRIO_STACK
#define PRIO_STACK

#include <map>
#include <stack>

template<class T, class K = int, class Comp = std::less<T> >
class priority_stack{
    using data_t = typename std::map<K, std::stack<T>, Comp>;
    data_t data;
    public:
        priority_stack(){}

        void push(const K& key, const T& value){
            data[key].push(value);
        };

        int size(const K& key) const{
            return data.find(key)->second.size();
        }

        int size() const{
            int s = 0;
            for(typename data_t::const_iterator it = data.begin(); it != data.end(); ++it){
                s += it->second.size();
            }
            return s;
        }

        T& top(){
            return data.rbegin()->second.top();
        }

        const T& top() const{
            return data.rbegin()->second.top();
        }

        void pop(){
            const K& key = data.rbegin()->first;
            data.rbegin()->second.pop();
            if(data.rbegin()->second.empty()) data.erase(key);
        }
};











#endif
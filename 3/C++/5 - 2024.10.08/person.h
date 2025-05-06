#ifndef PERSON_H
#define PERSON_H

#include <string>
#include <vector>

class Person{
    std::vector<Person*> friends;
    
public:
    int ID;
    double age;
    std::string name;

    Person(const std::string& name, double age, int id);

    bool is_friend(const Person& p);
};
#endif
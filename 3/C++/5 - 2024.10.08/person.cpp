#include "person.h"
#include <algorithm> //ezt ide include-oljuk és ne a header-be, ezzel fordítási időt spórolunk

Person::Person(const std::string& name, double age, int id): name(name), age(age), ID(id){}

bool Person::is_friend(const Person& p){
    return std::find(friends.begin(), friends.end(), &p) != friends.end();
}
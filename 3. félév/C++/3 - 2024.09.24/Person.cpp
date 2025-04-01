#include <algorithm>
#include <string>
#include <vector>
#include <set>
#include <iostream>

struct Person {
    std::string name;
    double age;
    int id;
    std::vector<Person *> friends;

    Person(std::string name, double age, int id){
        this->name = name;
        this->age = age;
        this->id = id;
    }

    bool is_friend(Person *person) {
        // Equivalence based on pointer equality
        //
        // [Indexing]
        // ==========================================================
        // for (std::size_t i = 0; i < friends.size(); ++i) {
        //   if (friends[i] == person) {
        //     return true;
        //   }
        // }
        // return false;
        // ==========================================================
        //
        // [Algorithm]
        // ==========================================================
        // return std::count(friends.begin(), friends.end(), person);
        //
        // [Range-based for loop]
        // ==========================================================
        // for (auto f : friends) {
        //   if (person == f) {
        //     return true;
        //   }
        // }
        // return false;
        // ==========================================================
        //
        // ----------------------------------------------------------
        // Equivalence based on id equality
        // ==========================================================
        // for (std::size_t i = 0; i < friends.size(); ++i) {
        //   if (friends[i]->id == person->id) {
        //     return true;
        //   }
        // }
        // return false;
        // ==========================================================
        // return std::count_if(friends.begin(), friends.end(),
        //                      [person](Person *p) { return person->id == p->id; });
        // ==========================================================

        for (auto f : friends) {
            if (f->id == person->id) {
                return true;
            }
        }
        return false;
  }

    // TODO: Implement the Person class
    // A Person should have a name and and age, and an id number
    // It should know its list of friends, and it should be able tell if another
    // Person is its friend or not. You should also implement function that lists
    // the transitive friend relationships of a person, so the results of this
    // function should be a list of Persons, who are freinds with the given
    // person, their friends and their friends etc.
    //make a list of 'known' and 'unknown' people, of whom we either know or dont know the firend of. then we add the friends of the unknown people to the unknown list, add them to the known, and remove the known from the unknown. repeat this process untill the unkonw list is empty, then return the know lis
};

std::vector<Person*> transitive_friends_list(const Person& person) {
    std::set<Person *> k;
    std::set<Person *> u;
    std::vector<Person *> list;

    for (auto f : person.friends) u.insert(f);
    while (!u.empty())
    {
        std::set<Person *> temp;
        for (auto p : u)
        {
            k.insert(p);
            for (auto f : p->friends) temp.insert(f);
        }
        u.clear();
        for (auto p : temp) if(auto search = k.find(p); search == k.end()) u.insert(p);
    }

    for(auto f: k) list.push_back(f);
    
    return list;
}

int main() {
    Person p1("Alice", 25, 1);
    Person p2("Bob", 30, 2);
    Person p3("Charlie", 35, 3);
    Person p4("Dave", 40, 4);
    Person p5("Eve", 45, 5);
    Person p6("Frank", 50, 6);

    p1.friends.push_back(&p2);
    p2.friends.push_back(&p3);
    p2.friends.push_back(&p1);
    p2.friends.push_back(&p6);
    p3.friends.push_back(&p6);
    p4.friends.push_back(&p5);

    std::vector<Person*> friends = transitive_friends_list(p1);
    std::cout << "[";
    for (auto p : friends) std::cout << " " << p->name;
    std::cout << " ]" << std::endl;

    friends = transitive_friends_list(p4);
    std::cout << "[";
    for (auto p : friends) std::cout << " " << p->name;
    std::cout << " ]" << std::endl;

    // transitive_friends_list(p1) should return [&p1, &p2, &p3, &p6], &p1 because p2 has it in its list
    // transitive_friends_list(p4) should return [&p5]
}
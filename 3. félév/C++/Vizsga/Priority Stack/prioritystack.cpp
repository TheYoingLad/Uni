/*

#include <stack>
#include <map>
#include <string>
#include <vector>
#include <iostream>

template<class T, class K = int, class Comp = std::less<T> >
class priority_stack {
	typedef typename std::map<K, std::stack<T>, Comp> prs;
	prs ps;
public:
	void push(const K& p, const T& t);
	int size(const K& p) const;
	T& top();
	const T& top() const;
	int size() const;
	void pop();
};

template<class T, class K, class Comp>
void priority_stack<T, K, Comp>::push(const K& p, const T& t) {
	ps[p].push(t);
}

template<class T, class K, class Comp>
int priority_stack<T, K, Comp>::size(const K& p) const {
	// return ps[p].size();
  /// ps[p] = 123;
	return ps.find(p)->second.size();
  // typename prs::iterator it = ps.find(p);
	// f (it != ps.end())
  //   return it->second.size();
  // return -1;
}

template<class T, class K, class Comp>
T& priority_stack<T, K, Comp>::top() {
	return ps.rbegin()->second.top();
}

template<class T, class K, class Comp>
const T& priority_stack<T, K, Comp>::top() const {
	return ps.rbegin()->second.top();
}

template<class T, class K, class Comp>
int priority_stack<T, K, Comp>::size() const {
	int s = 0;
	for (typename prs::const_iterator it = ps.begin(); it != ps.end(); ++it) {
		s+= it->second.size();
	}
	return s;
}

template<class T, class K, class Comp>
void priority_stack<T, K, Comp>::pop() {
	const K p = ps.rbegin()->first;
	ps.rbegin()->second.pop();
	if (ps.rbegin()->second.empty())
		ps.erase(p);
}
*/

#include <string>
#include <vector>
#include <iostream>

#include "prioritystack.h"
#include "prioritystack.h"

const int max = 1000;

struct StringSizeLess
{
	bool operator() (const std::string& a, const std::string& b) const { return a.size() < b.size(); }
};

template <typename T>
struct VectorSizeLess
{
	bool operator() (const std::vector<T>& a, const std::vector<T>& b) const { return a.size() < b.size(); }
};

int main() {
	int yourMark = 1;
	priority_stack<double> sd;
	for (int i = 0; i < max; ++i)
		sd.push(i, 6.7);

	priority_stack<int> si;
	si.push(5, 2);
	si.push(5, 3);
	si.push(4, 7);

	const priority_stack<int> csi = si;

	if (1 == sd.size(0) && 3 == si.top())
		yourMark = si.size(5);

	si.top() = 5;
	if (5 == si.top())
		yourMark = csi.size();

	si.pop();
	si.pop();

	priority_stack<int, double> sid;
	sid.push(6.4, 4);

	if (si.size() == sid.size())
		yourMark = sid.top();

	priority_stack<int, std::string, StringSizeLess> strst;
	strst.push("abcdg", 5);
	strst.push("dfg", 4);
	if (2 == strst.size())
		yourMark = strst.top();
	
	std::cout << "Your mark is: " <<  yourMark << "\n";

	priority_stack<int, std::vector<bool>, VectorSizeLess<bool> > bvst;
}
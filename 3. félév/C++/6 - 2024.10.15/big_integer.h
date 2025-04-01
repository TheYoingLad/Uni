#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <iterator>
#include <ostream>

/// Arbitrary precision integer type
class APInteger {
  std::vector<char> digits;
  bool m_is_negative;
public:
  APInteger(int64_t value): /*digits() helyes inicializálás, ez impicit*/ m_is_negative(value < 0) {
    value = std::abs(value);
    digits.reserve(std::ceil(std::log10(value))); //megéri-e? lebegőpontos számolások...
    while (value){
      digits.push_back(value % 10);
      value /= 10;
    }
  }
  APInteger(const std::string& value) : m_is_negative(!value.empty() && value.front() == '-'){
    digits.resize(value.size()); //0-ra inicializálás, implicit hívja a reserve()-t
    std::reverse_copy(value.begin(), value.end(), digits.begin());

    /*B:
    //reverse begin, end
    digits.resize(value.size());
    std::copy(value.rbegin(), value.rend(), digits.begin());
    */

    /*C:
    //nem inicializál, nem lehet hivatokzni az elemekre
    digits.reserve(value.size());
    std::reverse_copy(value.begin(), value.end(), std::back_inserter(digits));
    */
  }

  //                    V => rajta keresztül nem lehet változtatni ezt (this) az objektumot => ebben a fgv-ben a this const APInteger* típusú
  bool is_negative() const { return m_is_negative; }

  std::string to_string() const;
  int64_t to_int() const;

  // nem kell példányosítani a megvalósításhoz
  friend std::ostream& operator<<(std::ostream& os, const APInteger& integer){
    std::reverse_copy(integer.digits.begin(), integer.digits.end(), std::ostream_iterator<char>(os));
    
    return os;
  }
  
  friend APInteger operator+(const APInteger& lhs, const APInteger& rhs){
    std::string result;
    result.reserve(std::max(lhs.digits.size(), rhs.digits.size()) + 1);

    char carry = 0;

    for(size_t i = 0; i < lhs.digits.size() || i < rhs.digits.size(); i++){
      int64_t sum = lhs.digits[i] + rhs.digits[i] + carry;
      result.push_back(sum % 10);
      carry = sum / 10;
    }

    return result;
  }
};


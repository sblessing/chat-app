#pragma once
#include <cmath>
#include <limits>
#include <random>
#include <vector>

class pseudo_random {
public:
  pseudo_random(uint64_t seed) : value_(seed) {
    // nop
  }

  pseudo_random() = default;

  void set_seed(uint64_t seed) {
    value_ = seed;
  }

  uint32_t next_int(uint32_t exclusive_max = 0) {
    if (exclusive_max == 0)
      return static_cast<uint32_t>(next_long());
    else
      return static_cast<uint32_t>(next_long()) % exclusive_max;
  }

  uint64_t next() {
    return next_long();
  }

  uint64_t next_long() {
    value_ = ((value_ * 1309) + 13849) & 65535;
    return value_;
  }

  double next_double() {
    return 1.0 / static_cast<double>(next_long() + 1);
  }

  template <class T>
  void shuffle(std::vector<T>& vec) {
    for (auto i = vec.size(); i > 0; --i)
      std::swap(vec[i - 1], vec[next_int(i)]);
  }

private:
  uint64_t value_ = 74755;
};

#pragma once
#include <cmath>
#include <limits>
#include <random>
#include <vector>

class pseudo_random {
public:
  pseudo_random(std::uint64_t seed) : value_(seed) {
    // nop
  }

  pseudo_random() = default;

  void set_seed(std::uint64_t seed) {
    value_ = seed;
  }

  std::uint32_t next_int(int exclusive_max = 0) {
    if (exclusive_max == 0)
      return next_long();
    else
      return next_long() % exclusive_max;
  }

  std::uint64_t next_long() {
    value_ = ((value_ * 1309) + 13849) & 65535;
    return value_;
  }

  double next_double() {
    return 1.0 / (next_long() + 1);
  }

  bool next_boolean() {
    return next_int(2) != 0;
  }

  template <class T>
  void shuffle(std::vector<T>& vec) {
    for (auto i = vec.size(); i > 0; --i)
      std::swap(vec[i - 1], vec[next_int(i)]);
  }

  /// Pseudo next gaussian implemented after
  /// https://docs.oracle.com/javase/7/docs/api/java/util/Random.html
  double next_gaussian() {
    if (have_next_next_gaussian) {
      have_next_next_gaussian = false;
      return next_next_gaussian;
    } else {
      double v1, v2, s;
      do {
        v1 = 2 * next_double() - 1; // between -1.0 and 1.0
        v2 = 2 * next_double() - 1; // between -1.0 and 1.0
        s = v1 * v1 + v2 * v2;
      } while (s >= 1 || s == 0);
      double multiplier = sqrt(-2 * log(s) / s);
      next_next_gaussian = v2 * multiplier;
      have_next_next_gaussian = true;
      return v1 * multiplier;
    }
  }

private:
  std::uint64_t value_ = 74755;
  double next_next_gaussian = 0;
  bool have_next_next_gaussian = false;
};

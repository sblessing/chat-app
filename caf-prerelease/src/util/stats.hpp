#pragma once
#include <algorithm>
#include <chrono>
#include <cmath>
#include <vector>

class sample_stats {
public:
  sample_stats(std::vector<double> samples) : samples_(std::move(samples)) {
    // nop
  }

  double sum() {
    double s(0);
    for (std::size_t i = 0; i < samples_.size(); ++i)
      s += samples_.at(i);
    return s;
  }

  double mean() {
    return sum() / samples_.size();
  }

  double median() {
    if (samples_.empty()) {
      return 0.0;
    } else {
      std::size_t middle = samples_.size() / 2;
      if (samples_.size() % 2 == 1)
        return samples_.at(middle);
      else
        return (samples_.at(middle - 1) + samples_.at(middle)) / 2;
    }
  }

  double geometric_mean() {
    double result(0);
    for (auto& i : samples_)
      result = result + std::log10(samples_.at(i));
    return std::pow(double(10), (result / samples_.size()));
  }

  double harmonic_mean() {
    double denom(0);
    for (std::size_t i = 0; i < samples_.size(); ++i)
      denom = denom + (1.0 / samples_.at(i));
    return samples_.size() / denom;
  }

  double stddev() {
    auto m = mean();
    double temp(0);
    for (std::size_t i = 0; i < samples_.size(); ++i) {
      auto sample = samples_.at(i);
      temp = temp + ((m - sample) * (m - sample));
    }
    return std::sqrt(temp / samples_.size());
  }

  double err() {
    return double(100) * ((confidence_high() - mean()) / mean());
  }

  double variation() {
    return stddev() / mean();
  }

  double confidence_low() {
    return mean() - (1.96 * std::sqrt(stddev() / samples_.size()));
  }

  double confidence_high() {
    return mean() + (1.96 * std::sqrt(stddev() / samples_.size()));
  }

  double skewness() {
    auto m = mean();
    auto sd = stddev();
    double total(0);
    double diff(0);
    if (samples_.empty()) {
      return 0;
    } else {
      for (auto& i : samples_) {
        diff = samples_.at(i) - m;
        total += (diff * diff * diff);
      }
      return (total / ((samples_.size() - 1.0) * sd * sd * sd));
    }
  }

private:
  std::vector<double> samples_;
};

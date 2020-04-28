#pragma once

#include "pseudo_random.hpp"

class dice_roll {
public:
  dice_roll()
    : random_(42){
      // nop
    };

  explicit dice_roll(pseudo_random rand) : random_(rand) {
    // nop
  }

  uint32_t apply() {
    return random_.next_int(100);
  }

private:
  pseudo_random random_;
};

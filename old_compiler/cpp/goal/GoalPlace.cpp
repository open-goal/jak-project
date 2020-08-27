#include "GoalPlace.h"

static std::shared_ptr<Place> none = std::make_shared<NonePlace>(TypeSpec(nullptr));

std::shared_ptr<Place> get_none() {
  return none;
}
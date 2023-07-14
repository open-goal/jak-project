#include "texture_slots.h"

namespace {
std::vector<std::string> jak2_slots = {
    "jakbsmall-eyebrow",
    "jakbsmall-face",
    "jakbsmall-finger",
    "jakbsmall-hair",
};

}

const std::vector<std::string>& jak2_animated_texture_slots() {
  return jak2_slots;
}

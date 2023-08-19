#include "texture_slots.h"

namespace {
std::vector<std::string> jak2_slots = {
    "jakbsmall-eyebrow",
    "jakbsmall-face",
    "jakbsmall-finger",
    "jakbsmall-hair",
    "jak-orig-arm-formorph",
    "jak-orig-eyebrow-formorph",
    "jak-orig-eyelid-formorph",
    "jak-orig-finger-formorph",
    "jakb-facelft",
    "jakb-facert",
    "jakb-hairtrans",
    "jakb-eyelid",
    "jakb-finger",
    "jakb-eyebrow",
    //"kor-eyeeffect-formorph",
    //"kor-hair-formorph",
    //"kor-head-formorph",
    //"kor-head-formorph-noreflect",
    //"kor-lowercaps-formorph",
    //"kor-uppercaps-formorph",
    "skull-gem-dest",
    "bomb-gradient",
    "cas-conveyor-dest",
    "cas-conveyor-dest-01",
    "cas-conveyor-dest-02",
    "cas-conveyor-dest-03",
    "security-env-dest",
    "security-dot-dest",
    "waterfall-dest",
    "dig-lava-01-dest",
    "stdmb-energy-wall-01-dest",
    "robotank-tread-l-dest",
    "robotank-tread-r-dest",
    "fort-roboscreen-dest",
    "squid-env-rim-dest",
    "krew-holo-dest",
    "cas-toxic-slime-dest",
    "cas-toxic-slime-scroll-dest",
};

}

const std::vector<std::string>& jak2_animated_texture_slots() {
  return jak2_slots;
}

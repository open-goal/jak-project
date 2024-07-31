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

std::vector<std::string> jak3_slots = {
    "skull-gem-dest",
    "jakc-arm",
    "jakc-eyebrow",
    "jakc-face",
    "jakc-finger",
    "jakc-hair",
    "jakchires-arm",
    "jakchires-eye",
    "jakchires-eyebrow",
    "jakchires-eyelid",
    "jakchires-facelft",
    "jakchires-facert",
    "jakchires-hair",
    // default-water
    "bomb-gradient",
    "blue-beam-dest",
    "lightjak-wings",
    "mushroom-dest",
    // default-warp
    "shield-env-rim-dest",
    // templea-water
    "templea-waterfall-dest",
    // templea-warp
    "holograph-env-rim-dest",
    // hanga-sprite
    "glider-ring-dest2",
    "glider-ring-dest",
    // foresta-water
    "fora-water-dest",
    "fora-waterfall-01-dest",
    "fora-water-wave-01-dest",
    // forestb-water
    "forb-water-dest",
    "forb-waterfall-01-dest",
    "forb-water-wave-01-dest",
    // lforplnt-pris
    "mh-gem-dest",
    // lmhcitya-tfrag
    "mhcitya-base-goo-01-dest",
    // lmhcityb-tfrag
    "mhcityb-base-goo-01-dest",
    // mhcitya-pris
    // "mhcity-de-door-skin-01-dest",
    // templec-water
    "tplc-water-dest",
    // sewc-water
    "sewer-water-01-c-dest",
    "sewer-waterfall-01-c-dest",
    "sewer-waterfall-02-c-dest",
    "sewer-water-wave-01-c-dest",
    "sewer-water-highlight-01-c-dest",
    // sewd-water
    "sewer-water-01-d-dest",
    "sewer-waterfall-02-d-dest",
    "sewer-water-highlight-01-d-dest",
    "sewer-water-wave-01-d-dest",
    "sewer-water-wave-02-d-dest",
    "sewer-water-still-01-d-dest",
    // sewe-water
    "sewer-water-01-e-dest",
    "sewer-waterfall-01-e-dest",
    "sewer-waterfall-02-e-dest",
    "sewer-water-highlight-01-e-dest",
    // sewg-water
    "sewer-water-01-g-dest",
    "sewer-waterfall-02-g-dest",
    "sewer-water-wave-01-g-dest",
    // sewh-water
    "sewer-water-01-h-dest",
    "sewer-waterfall-02-h-dest",
    "sewer-water-wave-02-h-dest",
    "sewer-watefall-froth-01-h-dest",
    // sewi-water
    "sewer-water-still-01-i-dest",
    "sewer-waterfall-02-i-dest",
    "sewer-water-wave-01-i-dest",
    // sewj-water
    "sewer-waterfall-02-j-dest",
    "sewer-watefall-froth-01-j-dest",
    // sewl-water
    "sewer-waterfall-02-l-dest",
    "sewer-watefall-froth-01-l-dest",
    // sewm-water
    "sewer-water-01-m-dest",
    "sewer-waterfall-01-m-dest",
    "sewer-waterfall-02-m-dest",
    "sewer-water-highlight-01-m-dest",
    "sewer-water-wave-01-m-dest",
    "sewer-water-still-01-m-dest",
    "sewer-watefall-froth-01-m-dest",
    // sewn-water
    "sewer-waterfall-01-n-dest",
    "sewer-waterfall-02-n-dest",
    // "sewer-water-highlight-01-n-dest",
    "sewer-water-wave-01-n-dest",
    "sewer-water-still-01-n-dest",
    // hanga-water
    // "des-thermal-01-dest",
    // desresc-warp
    "sat-shield-dest",
    // security
    "security-env-dest",
    "security-dot-dest",
    // lgunnorm-water
    "kg-target-c-forcefield-01-dest",
    // templex-water
    "temple-waterfall-dest",
    // desertd-water
    "des-waterfall-dest",
    // towerb-water
    "tow-energy-bridge-dest",
    // factoryb-water
    "hemi-gradient-dest",
    "hemi-gradient-flames-dest",
    // factoryc-alpha
    "facc-convey-dest",
    "facc-convey-02-dest",
    // waspal-water
    "waspala-water-dest",
    "waspala-waterfall-dest",
    // rubblea-water
    "rub-water-dest",
    // rubblea2-water
    "rub-water-desta2",
    // rubbleb-water
    "rub-water-destb",
    // rubblec-water
    "rub-water-destc",
    // nstb-quicksand
    "nstb-quicksand-dest",
    // ctyslumb-water
    "ctyslumb-water-dest",
    "ctyslumb-fountain-fall-dest",
    // ctyslumc-water
    "ctyslumc-water-dest",
    "ctyslumc-fountain-fall-dest",
    // mined-tfrag
    "mined-pillar-side-dest",
    "mined-pillar-top2side-dest",
    "mined-pillar-top-dest",
    // volcanoa-alpha
    "vola-lava-01-dest",
    "vola-lava-fall-dest",
    // wasstada-alpha
    "wstd-lava-base-dest",
};

}  // namespace

const std::vector<std::string>& jak2_animated_texture_slots() {
  return jak2_slots;
}

const std::vector<std::string>& jak3_animated_texture_slots() {
  return jak3_slots;
}

#pragma once
#include "common/common_types.h"
#include "common/math/Vector.h"

struct PatSurface {
  enum class Mode { GROUND = 0, WALL = 1, OBSTACLE = 2, MAX_MODE = 3 };
  enum class Material {
    STONE = 0,
    ICE = 1,
    QUICKSAND = 2,
    WATERBOTTOM = 3,
    TAR = 4,
    SAND = 5,
    WOOD = 6,
    GRASS = 7,
    PCMETAL = 8,
    SNOW = 9,
    DEEPSNOW = 10,
    HOTCOALS = 11,
    LAVA = 12,
    CRWOOD = 13,
    GRAVEL = 14,
    DIRT = 15,
    METAL = 16,
    STRAW = 17,
    TUBE = 18,
    SWAMP = 19,
    STOPPROJ = 20,
    ROTATE = 21,
    NEUTRAL = 22,
    MAX_MATERIAL = 23
  };

  enum class Event {
    NONE = 0,
    DEADLY = 1,
    ENDLESSFALL = 2,
    BURN = 3,
    DEADLYUP = 4,
    BURNUP = 5,
    MELT = 6,
    MAX_EVENT = 7,
  };

  void set_noentity(bool x) {
    if (x) {
      val |= (1 << 0);
    } else {
      val &= ~(1 << 0);
    }
  }
  bool get_noentity() const { return val & (1 << 0); }

  void set_nocamera(bool x) {
    if (x) {
      val |= (1 << 1);
    } else {
      val &= ~(1 << 1);
    }
  }
  bool get_nocamera() const { return val & (1 << 1); }

  void set_noedge(bool x) {
    if (x) {
      val |= (1 << 2);
    } else {
      val &= ~(1 << 2);
    }
  }
  bool get_noedge() const { return val & (1 << 2); }

  void set_mode(Mode mode) {
    val &= ~(0b111 << 3);
    val |= ((u32)mode << 3);
  }
  Mode get_mode() const { return (Mode)(0b111 & (val >> 3)); }

  void set_material(Material mat) {
    val &= ~(0b111111 << 6);
    val |= ((u32)mat << 6);
  }
  Material get_material() const { return (Material)(0b111111 & (val >> 6)); }

  void set_nolineofsight(bool x) {
    if (x) {
      val |= (1 << 12);
    } else {
      val &= ~(1 << 12);
    }
  }
  bool get_nolineofsight() const { return val & (1 << 12); }

  void set_event(Event ev) {
    val &= ~(0b111111 << 14);
    val |= ((u32)ev << 14);
  }
  Event get_event() const { return (Event)(0b111111 & (val >> 14)); }

  bool operator==(const PatSurface& other) const { return val == other.val; }
  // bits 13, [15-31] are unused, or have unknown purpose.
  u32 val = 0;
};

struct CollideVertex {
  float x, y, z;
};

struct CollideFace {
  math::Vector4f bsphere;
  math::Vector3f v[3];
  PatSurface pat;
};

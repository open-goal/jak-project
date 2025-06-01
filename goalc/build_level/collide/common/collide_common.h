#pragma once
#include "common/common_types.h"
#include "common/math/Vector.h"
#include "common/util/Assert.h"

struct CollideVertex {
  float x, y, z;
};

namespace jak1 {
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

struct CollideFace {
  math::Vector4f bsphere;
  math::Vector3f v[3];
  PatSurface pat;
};
}  // namespace jak1

namespace jak2 {
struct PatSurface {
  enum class Mode { GROUND = 0, WALL = 1, OBSTACLE = 2, HALFPIPE = 3, MAX_MODE = 4 };
  enum class Material {
    NONE = 0,
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
    STONE = 23,
    CRMETAL = 24,
    CARPET = 25,
    GRMETAL = 26,
    SHMETAL = 27,
    HDWOOD = 28,
    MAX_MATERIAL = 29
  };

  enum class Event {
    NONE = 0,
    DEADLY = 1,
    ENDLESSFALL = 2,
    BURN = 3,
    DEADLYUP = 4,
    BURNUP = 5,
    MELT = 6,
    SLIDE = 7,
    LIP = 8,
    LIPRAMP = 9,
    SHOCK = 10,
    SHOCKUP = 11,
    HIDE = 12,
    RAIL = 13,
    SLIPPERY = 14,
    MAX_EVENT = 15,
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

  void set_nogrind(bool x) {
    if (x) {
      val |= (1 << 3);
    } else {
      val &= ~(1 << 3);
    }
  }
  bool get_nogrind() const { return val & (1 << 3); }

  void set_nojak(bool x) {
    if (x) {
      val |= (1 << 4);
    } else {
      val &= ~(1 << 4);
    }
  }
  bool get_nojak() const { return val & (1 << 4); }

  void set_noboard(bool x) {
    if (x) {
      val |= (1 << 5);
    } else {
      val &= ~(1 << 5);
    }
  }
  bool get_noboard() const { return val & (1 << 5); }

  void set_nopilot(bool x) {
    if (x) {
      val |= (1 << 6);
    } else {
      val &= ~(1 << 6);
    }
  }
  bool get_nopilot() const { return val & (1 << 6); }

  void set_mode(Mode mode) {
    val &= ~(0b111 << 7);
    val |= ((u32)mode << 7);
  }
  Mode get_mode() const { return (Mode)(0b111 & (val >> 7)); }

  void set_material(Material mat) {
    val &= ~(0b111111 << 10);
    val |= ((u32)mat << 10);
  }
  Material get_material() const { return (Material)(0b111111 & (val >> 10)); }

  void set_nolineofsight(bool x) {
    if (x) {
      val |= (1 << 16);
    } else {
      val &= ~(1 << 16);
    }
  }
  bool get_nolineofsight() const { return val & (1 << 16); }

  void set_event(Event ev) {
    val &= ~(0b111111 << 18);
    val |= ((u32)ev << 18);
  }
  Event get_event() const { return (Event)(0b111111 & (val >> 18)); }

  void set_probe(bool x) {
    if (x) {
      val |= (1 << 24);
    } else {
      val &= ~(1 << 24);
    }
  }
  bool get_probe() const { return val & (1 << 24); }

  void set_nomech(bool x) {
    if (x) {
      val |= (1 << 25);
    } else {
      val &= ~(1 << 25);
    }
  }
  bool get_nomech() const { return val & (1 << 25); }

  void set_noproj(bool x) {
    if (x) {
      val |= (1 << 26);
    } else {
      val &= ~(1 << 26);
    }
  }
  bool get_noproj() const { return val & (1 << 26); }

  void set_noendlessfall(bool x) {
    if (x) {
      val |= (1 << 27);
    } else {
      val &= ~(1 << 27);
    }
  }
  bool get_noendlessfall() const { return val & (1 << 27); }

  void set_noprobe(bool x) {
    if (x) {
      val |= (1 << 28);
    } else {
      val &= ~(1 << 28);
    }
  }
  bool get_noprobe() const { return val & (1 << 28); }

  bool operator==(const PatSurface& other) const { return val == other.val; }
  u32 val = 0;
};

struct CollideFace {
  math::Vector3f v[3];
  PatSurface pat;
};
}  // namespace jak2

namespace jak3 {
struct PatSurface {
  enum class Mode { GROUND = 0, WALL = 1, OBSTACLE = 2, HALFPIPE = 3, MAX_MODE = 4 };
  enum class Material {
    NONE = 0,
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
    STONE = 23,
    CRMETAL = 24,
    CARPET = 25,
    GRMETAL = 26,
    SHMETAL = 27,
    HDWOOD = 28,
    SQUISH = 29,
    MHSHROOM = 30,
    FOREST = 31,
    MHSWAMP = 32,
    DMAKER = 33,
    MAX_MATERIAL = 34
  };

  enum class Event {
    NONE = 0,
    DEADLY = 1,
    ENDLESSFALL = 2,
    BURN = 3,
    DEADLYUP = 4,
    BURNUP = 5,
    MELT = 6,
    SLIDE = 7,
    LIP = 8,
    LIPRAMP = 9,
    SHOCK = 10,
    SHOCKUP = 11,
    HIDE = 12,
    RAIL = 13,
    SLIPPERY = 14,
    DRAG = 15,
    WATERFLOOR = 16,
    HANG = 17,
    FRY = 18,
    SLIME = 19,
    MAX_EVENT = 20
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

  void set_nogrind(bool x) {
    if (x) {
      val |= (1 << 3);
    } else {
      val &= ~(1 << 3);
    }
  }
  bool get_nogrind() const { return val & (1 << 3); }

  void set_nojak(bool x) {
    if (x) {
      val |= (1 << 4);
    } else {
      val &= ~(1 << 4);
    }
  }
  bool get_nojak() const { return val & (1 << 4); }

  void set_noboard(bool x) {
    if (x) {
      val |= (1 << 5);
    } else {
      val &= ~(1 << 5);
    }
  }
  bool get_noboard() const { return val & (1 << 5); }

  void set_nopilot(bool x) {
    if (x) {
      val |= (1 << 6);
    } else {
      val &= ~(1 << 6);
    }
  }
  bool get_nopilot() const { return val & (1 << 6); }

  void set_mode(Mode mode) {
    val &= ~(0b111 << 7);
    val |= ((u32)mode << 7);
  }
  Mode get_mode() const { return (Mode)(0b111 & (val >> 7)); }

  void set_material(Material mat) {
    val &= ~(0b111111 << 10);
    val |= ((u32)mat << 10);
  }
  Material get_material() const { return (Material)(0b111111 & (val >> 10)); }

  void set_nolineofsight(bool x) {
    if (x) {
      val |= (1 << 16);
    } else {
      val &= ~(1 << 16);
    }
  }
  bool get_nolineofsight() const { return val & (1 << 16); }

  void set_event(Event ev) {
    val &= ~(0b111111 << 18);
    val |= ((u32)ev << 18);
  }
  Event get_event() const { return (Event)(0b111111 & (val >> 18)); }

  void set_probe(bool x) {
    if (x) {
      val |= (1 << 24);
    } else {
      val &= ~(1 << 24);
    }
  }
  bool get_probe() const { return val & (1 << 24); }

  void set_nomech(bool x) {
    if (x) {
      val |= (1 << 25);
    } else {
      val &= ~(1 << 25);
    }
  }
  bool get_nomech() const { return val & (1 << 25); }

  void set_noproj(bool x) {
    if (x) {
      val |= (1 << 26);
    } else {
      val &= ~(1 << 26);
    }
  }
  bool get_noproj() const { return val & (1 << 26); }

  void set_noendlessfall(bool x) {
    if (x) {
      val |= (1 << 27);
    } else {
      val &= ~(1 << 27);
    }
  }
  bool get_noendlessfall() const { return val & (1 << 27); }

  void set_noprobe(bool x) {
    if (x) {
      val |= (1 << 28);
    } else {
      val &= ~(1 << 28);
    }
  }
  bool get_noprobe() const { return val & (1 << 28); }

  void set_board(bool x) {
    if (x) {
      val |= (1 << 4);
    } else {
      val &= ~(1 << 4);
    }
  }
  bool get_board() const { return val & (1 << 4); }

  bool operator==(const PatSurface& other) const { return val == other.val; }
  u32 val = 0;
};

struct CollideFace {
  math::Vector3f v[3];
  PatSurface pat;
};
}  // namespace jak3

/*!
 * Convert jak1-format PatSurface to Jak 2.
 */
inline jak2::PatSurface jak2_pat(jak1::PatSurface jak1) {
  jak2::PatSurface result;

  switch (jak1.get_mode()) {
    case jak1::PatSurface::Mode::GROUND:
      result.set_mode(jak2::PatSurface::Mode::GROUND);
      break;
    case jak1::PatSurface::Mode::WALL:
      result.set_mode(jak2::PatSurface::Mode::WALL);
      break;
    case jak1::PatSurface::Mode::OBSTACLE:
      result.set_mode(jak2::PatSurface::Mode::OBSTACLE);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_material()) {
    case jak1::PatSurface::Material::STONE:
      result.set_material(jak2::PatSurface::Material::STONE);
      break;
    case jak1::PatSurface::Material::ICE:
      result.set_material(jak2::PatSurface::Material::ICE);
      break;
    case jak1::PatSurface::Material::QUICKSAND:
      result.set_material(jak2::PatSurface::Material::QUICKSAND);
      break;
    case jak1::PatSurface::Material::WATERBOTTOM:
      result.set_material(jak2::PatSurface::Material::WATERBOTTOM);
      break;
    case jak1::PatSurface::Material::TAR:
      result.set_material(jak2::PatSurface::Material::TAR);
      break;
    case jak1::PatSurface::Material::SAND:
      result.set_material(jak2::PatSurface::Material::SAND);
      break;
    case jak1::PatSurface::Material::WOOD:
      result.set_material(jak2::PatSurface::Material::WOOD);
      break;
    case jak1::PatSurface::Material::GRASS:
      result.set_material(jak2::PatSurface::Material::GRASS);
      break;
    case jak1::PatSurface::Material::PCMETAL:
      result.set_material(jak2::PatSurface::Material::PCMETAL);
      break;
    case jak1::PatSurface::Material::SNOW:
      result.set_material(jak2::PatSurface::Material::SNOW);
      break;
    case jak1::PatSurface::Material::DEEPSNOW:
      result.set_material(jak2::PatSurface::Material::DEEPSNOW);
      break;
    case jak1::PatSurface::Material::HOTCOALS:
      result.set_material(jak2::PatSurface::Material::HOTCOALS);
      break;
    case jak1::PatSurface::Material::LAVA:
      result.set_material(jak2::PatSurface::Material::LAVA);
      break;
    case jak1::PatSurface::Material::CRWOOD:
      result.set_material(jak2::PatSurface::Material::CRWOOD);
      break;
    case jak1::PatSurface::Material::GRAVEL:
      result.set_material(jak2::PatSurface::Material::GRAVEL);
      break;
    case jak1::PatSurface::Material::DIRT:
      result.set_material(jak2::PatSurface::Material::DIRT);
      break;
    case jak1::PatSurface::Material::METAL:
      result.set_material(jak2::PatSurface::Material::METAL);
      break;
    case jak1::PatSurface::Material::STRAW:
      result.set_material(jak2::PatSurface::Material::STRAW);
      break;
    case jak1::PatSurface::Material::TUBE:
      result.set_material(jak2::PatSurface::Material::TUBE);
      break;
    case jak1::PatSurface::Material::SWAMP:
      result.set_material(jak2::PatSurface::Material::SWAMP);
      break;
    case jak1::PatSurface::Material::STOPPROJ:
      result.set_material(jak2::PatSurface::Material::STOPPROJ);
      break;
    case jak1::PatSurface::Material::ROTATE:
      result.set_material(jak2::PatSurface::Material::ROTATE);
      break;
    case jak1::PatSurface::Material::NEUTRAL:
      result.set_material(jak2::PatSurface::Material::NEUTRAL);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_event()) {
    case jak1::PatSurface::Event::NONE:
      result.set_event(jak2::PatSurface::Event::NONE);
      break;
    case jak1::PatSurface::Event::DEADLY:
      result.set_event(jak2::PatSurface::Event::DEADLY);
      break;
    case jak1::PatSurface::Event::ENDLESSFALL:
      result.set_event(jak2::PatSurface::Event::ENDLESSFALL);
      break;
    case jak1::PatSurface::Event::BURN:
      result.set_event(jak2::PatSurface::Event::BURN);
      break;
    case jak1::PatSurface::Event::DEADLYUP:
      result.set_event(jak2::PatSurface::Event::DEADLYUP);
      break;
    case jak1::PatSurface::Event::BURNUP:
      result.set_event(jak2::PatSurface::Event::BURNUP);
      break;
    case jak1::PatSurface::Event::MELT:
      result.set_event(jak2::PatSurface::Event::MELT);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  result.set_noentity(jak1.get_noentity());
  result.set_nocamera(jak1.get_nocamera());

  result.set_noedge(jak1.get_noedge());
  result.set_nolineofsight(jak1.get_nolineofsight());

  return result;
}

/*!
 * Convert jak1-format PatSurface to Jak 3.
 */
inline jak3::PatSurface jak3_pat(jak1::PatSurface jak1) {
  jak3::PatSurface result;

  switch (jak1.get_mode()) {
    case jak1::PatSurface::Mode::GROUND:
      result.set_mode(jak3::PatSurface::Mode::GROUND);
      break;
    case jak1::PatSurface::Mode::WALL:
      result.set_mode(jak3::PatSurface::Mode::WALL);
      break;
    case jak1::PatSurface::Mode::OBSTACLE:
      result.set_mode(jak3::PatSurface::Mode::OBSTACLE);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_material()) {
    case jak1::PatSurface::Material::STONE:
      result.set_material(jak3::PatSurface::Material::STONE);
      break;
    case jak1::PatSurface::Material::ICE:
      result.set_material(jak3::PatSurface::Material::ICE);
      break;
    case jak1::PatSurface::Material::QUICKSAND:
      result.set_material(jak3::PatSurface::Material::QUICKSAND);
      break;
    case jak1::PatSurface::Material::WATERBOTTOM:
      result.set_material(jak3::PatSurface::Material::WATERBOTTOM);
      break;
    case jak1::PatSurface::Material::TAR:
      result.set_material(jak3::PatSurface::Material::TAR);
      break;
    case jak1::PatSurface::Material::SAND:
      result.set_material(jak3::PatSurface::Material::SAND);
      break;
    case jak1::PatSurface::Material::WOOD:
      result.set_material(jak3::PatSurface::Material::WOOD);
      break;
    case jak1::PatSurface::Material::GRASS:
      result.set_material(jak3::PatSurface::Material::GRASS);
      break;
    case jak1::PatSurface::Material::PCMETAL:
      result.set_material(jak3::PatSurface::Material::PCMETAL);
      break;
    case jak1::PatSurface::Material::SNOW:
      result.set_material(jak3::PatSurface::Material::SNOW);
      break;
    case jak1::PatSurface::Material::DEEPSNOW:
      result.set_material(jak3::PatSurface::Material::DEEPSNOW);
      break;
    case jak1::PatSurface::Material::HOTCOALS:
      result.set_material(jak3::PatSurface::Material::HOTCOALS);
      break;
    case jak1::PatSurface::Material::LAVA:
      result.set_material(jak3::PatSurface::Material::LAVA);
      break;
    case jak1::PatSurface::Material::CRWOOD:
      result.set_material(jak3::PatSurface::Material::CRWOOD);
      break;
    case jak1::PatSurface::Material::GRAVEL:
      result.set_material(jak3::PatSurface::Material::GRAVEL);
      break;
    case jak1::PatSurface::Material::DIRT:
      result.set_material(jak3::PatSurface::Material::DIRT);
      break;
    case jak1::PatSurface::Material::METAL:
      result.set_material(jak3::PatSurface::Material::METAL);
      break;
    case jak1::PatSurface::Material::STRAW:
      result.set_material(jak3::PatSurface::Material::STRAW);
      break;
    case jak1::PatSurface::Material::TUBE:
      result.set_material(jak3::PatSurface::Material::TUBE);
      break;
    case jak1::PatSurface::Material::SWAMP:
      result.set_material(jak3::PatSurface::Material::SWAMP);
      break;
    case jak1::PatSurface::Material::STOPPROJ:
      result.set_material(jak3::PatSurface::Material::STOPPROJ);
      break;
    case jak1::PatSurface::Material::ROTATE:
      result.set_material(jak3::PatSurface::Material::ROTATE);
      break;
    case jak1::PatSurface::Material::NEUTRAL:
      result.set_material(jak3::PatSurface::Material::NEUTRAL);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_event()) {
    case jak1::PatSurface::Event::NONE:
      result.set_event(jak3::PatSurface::Event::NONE);
      break;
    case jak1::PatSurface::Event::DEADLY:
      result.set_event(jak3::PatSurface::Event::DEADLY);
      break;
    case jak1::PatSurface::Event::ENDLESSFALL:
      result.set_event(jak3::PatSurface::Event::ENDLESSFALL);
      break;
    case jak1::PatSurface::Event::BURN:
      result.set_event(jak3::PatSurface::Event::BURN);
      break;
    case jak1::PatSurface::Event::DEADLYUP:
      result.set_event(jak3::PatSurface::Event::DEADLYUP);
      break;
    case jak1::PatSurface::Event::BURNUP:
      result.set_event(jak3::PatSurface::Event::BURNUP);
      break;
    case jak1::PatSurface::Event::MELT:
      result.set_event(jak3::PatSurface::Event::MELT);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  result.set_noentity(jak1.get_noentity());
  result.set_nocamera(jak1.get_nocamera());

  result.set_noedge(jak1.get_noedge());
  result.set_nolineofsight(jak1.get_nolineofsight());

  return result;
}
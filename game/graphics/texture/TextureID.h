#pragma once

#include "common/common_types.h"

struct PcTextureId {
  u16 page = -1;
  u16 tex = -1;

  PcTextureId(u16 p, u16 t) : page(p), tex(t) {}
  PcTextureId() = default;

  static PcTextureId from_combo_id(u32 val) { return PcTextureId(val >> 16, val & 0xffff); }

  bool operator==(const PcTextureId& other) const { return page == other.page && tex == other.tex; }
};

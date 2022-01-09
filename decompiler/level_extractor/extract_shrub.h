#pragma once

#include "decompiler/level_extractor/BspHeader.h"
#include "common/math/Vector.h"
#include "common/custom_data/Tfrag3Data.h"
#include "decompiler/data/TextureDB.h"

namespace decompiler {

namespace shrub_types {

struct Shrubbery : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "shrubbery"; }

  // 4 - textures
  u16 gif_count;
  std::vector<u8> gif_data;

  u32 header_unknown;  // 8
  u8 obj_qwc;          // 12
  u8 vtx_qwc;          // 13
  u8 col_qwc;          // 14
  u8 stq_qwc;          // 15

  u32 obj;   // 16
  u32 vtx;   // 20
  u32 col;   // 24
  u32 steq;  // 28
};

struct PrototypeShrubbery : public level_tools::DrawableInlineArray {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
  s16 id;      // 0
  s16 length;  // 4

  level_tools::Vector bsphere;    // 16
  std::vector<Shrubbery> shrubs;  // 32
};

struct PrototypeBucketShrub {
  std::string name;  // 4
  u32 flags;         // 8
  u16 in_level;      // 12
  u16 utextures;     // 14

  PrototypeShrubbery geometry[4];  // 16

  level_tools::Vector dists;  // 32
  // - near-plane
  // - near-stiff
  // - mid-plane
  // - far-plane
  level_tools::Vector rdists;  // 48
  // - rlength-near
  // - rlength-stiff
  // - rlength-mid
  // - stiffness

  u32 next[4];   // 64
  u16 count[4];  // 80 | NOTE - overlayed in places as a 128bit value

  u16 mod_count[4];  // 88
  Ref last[4];       // 96 | NOTE - overlayed to clear with a single uint128

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;
};

struct PrototypeArrayShrub {
  s16 id;                                  // 4
  s16 length;                              // 6
  level_tools::Vector bsphere;             // 16
  std::vector<PrototypeBucketShrub> data;  // 32

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;
};

struct PrototypeArrayShrubInfo {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats);
  std::string print(const level_tools::PrintSettings& settings, int indent) const;

  PrototypeArrayShrub prototype_array_shrub;
  // todo wind vectors.
};

struct InstanceShrubbery : public level_tools::Drawable {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override { return "instance-shrubbery"; }

  s16 id;                        // 4
  u16 bucket_index;              // 6
  level_tools::Vector bsphere;   // 16
  level_tools::Matrix4h origin;  // 32
  u16 flags;                     // ??
  u16 wind_index;                // 62

  // --- instance-shrubbery ---
  u32 color;                        // 8
  level_tools::Vector flat_normal;  // 64
  level_tools::Vector flat_hwidth;  // 76
};

struct DrawableInlineArrayInstanceShrub : public level_tools::DrawableInlineArray {
  s16 id;
  s16 length;
  level_tools::Vector bsphere;

  std::vector<InstanceShrubbery> instances;

  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override;
};

struct DrawableTreeInstanceShrub : public level_tools::DrawableTree {
  void read_from_file(TypedRef ref,
                      const decompiler::DecompilerTypeSystem& dts,
                      level_tools::DrawStats* stats) override;
  std::string print(const level_tools::PrintSettings& settings, int indent) const override;
  std::string my_type() const override;

  s16 id;                        // 0
  s16 length;                    // 4
  PrototypeArrayShrubInfo info;  // 8
  u32 colors_added;              // 12
  level_tools::Vector bsphere;   // 16

  std::vector<std::unique_ptr<level_tools::DrawableInlineArray>> arrays;
};

}  // namespace shrub_types

/// <summary>
/// Extract shrubs from the level
/// </summary>
/// <param name="tree"></param>
/// <param name="debug_name"></param>
/// <param name="map"></param>
/// <param name="tex_db"></param>
/// <param name="expected_missing_textures"></param>
/// <param name="out"></param>
/// <param name="dump_level"></param>
void extract_shrub(const shrub_types::DrawableTreeInstanceShrub* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   const std::vector<std::pair<int, int>>& expected_missing_textures,
                   tfrag3::Level& out,
                   bool dump_level);

}  // namespace decompiler

#include "common/util/Assert.h"
#include "common/util/DgoReader.h"
#include "common/util/FileUtil.h"
#include "common/util/unicode_util.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/ObjectFile/LinkedObjectFileCreation.h"
#include "decompiler/level_extractor/BspHeader.h"
#include "decompiler/util/goal_data_reader.h"

#include "third-party/fmt/core.h"

constexpr GameVersion kGameVersion = GameVersion::Jak1;

/*!
 * Get the level data from a DGO File.
 * Will ignore all the other things in the level DGO and just return the bsp file.
 */
decompiler::LinkedObjectFile load_bsp_from_dgo(const std::string& file_name,
                                               decompiler::DecompilerTypeSystem& dts) {
  std::string short_name = file_util::base_name(file_name);
  fmt::print("Loading DGO file: {}\n", short_name);
  auto dgo_file_data = file_util::read_binary_file(file_name);

  auto dgo = DgoReader(short_name, dgo_file_data);
  auto entries = dgo.entries();
  ASSERT(entries.size() > 0);

  const auto& level_file = entries.back();

  fmt::print("Using level file: {}, size {} kB\n", level_file.internal_name,
             level_file.data.size() / 1024);

  return decompiler::to_linked_object_file(level_file.data, level_file.internal_name, dts,
                                           kGameVersion);
}

bool is_valid_bsp(const decompiler::LinkedObjectFile& file) {
  if (file.segments != 1) {
    fmt::print("Got {} segments, but expected 1\n", file.segments);
    return false;
  }

  auto& first_word = file.words_by_seg.at(0).at(0);
  if (first_word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    fmt::print("Expected the first word to be a type pointer, but it wasn't.\n");
    return false;
  }

  if (first_word.symbol_name() != "bsp-header") {
    fmt::print("Expected to get a bsp-header, but got {} instead.\n", first_word.symbol_name());
    return false;
  }

  return true;
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  try {
    fmt::print("Level Dump Tool\n");

    if (argc != 2) {
      fmt::print("Usage: level_dump <path-to-dgo>\n");
      return 1;
    }

    fmt::print("Setting up types...\n");
    decompiler::DecompilerTypeSystem dts(kGameVersion);
    dts.parse_type_defs({"decompiler", "config", "all-types.gc"});

    std::string file_name = argv[1];

    fmt::print("Loading data...\n");
    auto data = load_bsp_from_dgo(file_name, dts);
    data.set_ordered_label_names();

    if (!is_valid_bsp(data)) {
      fmt::print("Invalid level file.\n");
      return 1;
    }

    level_tools::DrawStats draw_stats;
    // draw_stats.debug_print_dma_data = true;
    level_tools::BspHeader bsp_header;
    bsp_header.read_from_file(data, dts, &draw_stats, kGameVersion);

    level_tools::PrintSettings settings;
    fmt::print("{}\n", bsp_header.print(settings));
    fmt::print("Stats:\n{}\n", draw_stats.print());

  } catch (const std::exception& e) {
    fmt::print("Error: {}\n", e.what());
  }

  return 0;
}

#include <map>
#include <regex>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/read_iso_file.h"

#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/config.h"
#include "decompiler/level_extractor/extract_level.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"

enum class ExtractorErrorCode {
  SUCCESS = 0,
  VALIDATION_CANT_LOCATE_ELF = 4000,
  VALIDATION_SERIAL_MISSING_FROM_DB = 4001,
  VALIDATION_ELF_MISSING_FROM_DB = 4002,
  VALIDATION_BAD_ISO_CONTENTS = 4010,
  VALIDATION_INCORRECT_EXTRACTION_COUNT = 4011,
  VALIDATION_BAD_EXTRACTION = 4020,
  DECOMPILATION_GENERIC_ERROR = 4030
};

enum GameIsoFlags { FLAG_JAK1_BLACK_LABEL = (1 << 0) };

static const std::unordered_map<std::string, GameIsoFlags> sGameIsoFlagNames = {
    {"jak1-black-label", FLAG_JAK1_BLACK_LABEL}};

struct ISOMetadata {
  std::string canonical_name;
  std::string region;
  int num_files;
  xxh::hash64_t contents_hash;
  std::string decomp_config;
  GameIsoFlags flags = (GameIsoFlags)0;
};

// TODO - when we support jak2 and beyond, add which game it's for as well
// this will let the installer reject (or gracefully handle) jak2 isos on the jak1 page, etc.

// { SERIAL : { ELF_HASH : ISOMetadataDatabase } }
static const std::map<std::string, std::map<xxh::hash64_t, ISOMetadata>> isoDatabase{
    {"SCUS-97124",
     {{7280758013604870207U,
       {"Jak & Daxter™: The Precursor Legacy (Black Label)", "NTSC-U", 337, 11363853835861842434U,
        "jak1_ntsc_black_label", FLAG_JAK1_BLACK_LABEL}},
      {744661860962747854,
       {"Jak & Daxter™: The Precursor Legacy", "NTSC-U", 338, 8538304367812415885U, "jak1_jp"}}}},
    {"SCES-50361",
     {{12150718117852276522U,
       {"Jak & Daxter™: The Precursor Legacy", "PAL", 338, 16850370297611763875U, "jak1_pal"}}}},
    {"SCPS-15021",
     {{16909372048085114219U,
       {"ジャックＸダクスター　～　旧世界の遺産", "NTSC-J", 338, 1262350561338887717,
        "jak1_jp"}}}}};

void setup_global_decompiler_stuff(std::optional<std::filesystem::path> project_path_override) {
  decompiler::init_opcode_info();
  file_util::setup_project_path(project_path_override);
}

IsoFile extract_files(std::filesystem::path data_dir_path,
                      std::filesystem::path extracted_iso_path) {
  fmt::print("Note: input isn't a folder, assuming it's an ISO file...\n");

  std::filesystem::create_directories(extracted_iso_path);

  auto fp = fopen(data_dir_path.string().c_str(), "rb");
  ASSERT_MSG(fp, "failed to open input ISO file\n");
  IsoFile iso = unpack_iso_files(fp, extracted_iso_path, true, true);
  fclose(fp);
  return iso;
}

std::pair<std::optional<std::string>, std::optional<xxh::hash64_t>> findElfFile(
    const std::filesystem::path& extracted_iso_path) {
  std::optional<std::string> serial = std::nullopt;
  std::optional<xxh::hash64_t> elf_hash = std::nullopt;
  for (const auto& entry : fs::directory_iterator(extracted_iso_path)) {
    auto as_str = entry.path().filename().string();
    if (std::regex_match(as_str, std::regex(".{4}_.{3}\\..{2}"))) {
      serial = std::make_optional(
          fmt::format("{}-{}", as_str.substr(0, 4), as_str.substr(5, 3) + as_str.substr(9, 2)));
      // We already found the path, so hash it while we're here
      auto fp = fopen(entry.path().string().c_str(), "rb");
      fseek(fp, 0, SEEK_END);
      size_t size = ftell(fp);
      std::vector<u8> buffer(size);
      rewind(fp);
      fread(&buffer[0], sizeof(std::vector<u8>::value_type), buffer.size(), fp);
      elf_hash = std::make_optional(xxh::xxhash<64>(buffer));
      break;
    }
  }
  return {serial, elf_hash};
}

std::pair<ExtractorErrorCode, std::optional<ISOMetadata>> validate(
    const IsoFile& iso_file,
    const std::filesystem::path& extracted_iso_path) {
  if (!std::filesystem::exists(extracted_iso_path / "DGO")) {
    fmt::print(stderr, "ERROR: input folder doesn't have a DGO folder. Is this the right input?\n");
    return {ExtractorErrorCode::VALIDATION_BAD_EXTRACTION, std::nullopt};
  }

  std::optional<ExtractorErrorCode> error_code;
  std::optional<std::string> serial = std::nullopt;
  std::optional<xxh::hash64_t> elf_hash = std::nullopt;
  std::tie(serial, elf_hash) = findElfFile(extracted_iso_path);

  // - XOR all hashes together and hash the result.  This makes the ordering of the hashes (aka
  // files) irrelevant
  xxh::hash64_t combined_hash = 0;
  for (const auto& hash : iso_file.hashes) {
    combined_hash ^= hash;
  }
  xxh::hash64_t contents_hash = xxh::xxhash<64>({combined_hash});

  if (!serial || !elf_hash) {
    fmt::print(stderr, "ERROR: Unable to locate a Serial/ELF file!\n");
    if (!error_code.has_value()) {
      error_code = std::make_optional(ExtractorErrorCode::VALIDATION_CANT_LOCATE_ELF);
    }
    // No point in continuing here
    return {*error_code, std::nullopt};
  }

  // Find the game in our tracking database
  std::optional<ISOMetadata> meta_res = std::nullopt;
  if (auto dbEntry = isoDatabase.find(serial.value()); dbEntry == isoDatabase.end()) {
    fmt::print(stderr, "ERROR: Serial '{}' not found in the validation database\n", serial.value());
    if (!error_code.has_value()) {
      error_code = std::make_optional(ExtractorErrorCode::VALIDATION_SERIAL_MISSING_FROM_DB);
    }
  } else {
    auto& metaMap = dbEntry->second;
    auto meta_entry = metaMap.find(elf_hash.value());
    if (meta_entry == metaMap.end()) {
      fmt::print(stderr,
                 "ERROR: ELF Hash '{}' not found in the validation database, is this a new or "
                 "modified version of the same game?\n",
                 elf_hash.value());
      if (!error_code.has_value()) {
        error_code = std::make_optional(ExtractorErrorCode::VALIDATION_ELF_MISSING_FROM_DB);
      }
    } else {
      meta_res = std::make_optional<ISOMetadata>(meta_entry->second);
      const auto& meta = *meta_res;
      // Print out some information
      fmt::print("Detected Game Metadata:\n");
      fmt::print("\tDetected - {}\n", meta.canonical_name);
      fmt::print("\tRegion - {}\n", meta.region);
      fmt::print("\tSerial - {}\n", dbEntry->first);
      fmt::print("\tUses Decompiler Config - {}\n", meta.decomp_config);

      // - Number of Files
      if (meta.num_files != iso_file.files_extracted) {
        fmt::print(stderr,
                   "ERROR: Extracted an unexpected number of files. Expected '{}', Actual '{}'\n",
                   meta.num_files, iso_file.files_extracted);
        if (!error_code.has_value()) {
          error_code =
              std::make_optional(ExtractorErrorCode::VALIDATION_INCORRECT_EXTRACTION_COUNT);
        }
      }
      // Check the ISO Hash
      if (meta.contents_hash != contents_hash) {
        fmt::print(stderr,
                   "ERROR: Overall ISO content's hash does not match. Expected '{}', Actual '{}'\n",
                   meta.contents_hash, contents_hash);
      }
    }
  }

  // Finally, return the result
  if (error_code.has_value()) {
    // Generate the map entry to make things simple, just convienance
    if (error_code.value() == ExtractorErrorCode::VALIDATION_SERIAL_MISSING_FROM_DB) {
      fmt::print(
          "If this is a new release or version that should be supported, consider adding the "
          "following serial entry to the database:\n");
      fmt::print(
          "\t'{{\"{}\", {{{{{}U, {{\"GAME_TITLE\", \"NTSC-U/PAL/NTSC-J\", {}, {}U, "
          "\"DECOMP_CONFIF_FILENAME_NO_EXTENSION\"}}}}}}}}'\n",
          serial.value(), elf_hash.value(), iso_file.files_extracted, contents_hash);
    } else if (error_code.value() == ExtractorErrorCode::VALIDATION_ELF_MISSING_FROM_DB) {
      fmt::print(
          "If this is a new release or version that should be supported, consider adding the "
          "following ELF entry to the database under the '{}' serial:\n",
          serial.value());
      fmt::print(
          "\t'{{{}, {{\"GAME_TITLE\", \"NTSC-U/PAL/NTSC-J\", {}, {}U, "
          "\"DECOMP_CONFIF_FILENAME_NO_EXTENSION\"}}}}'\n",
          elf_hash.value(), iso_file.files_extracted, contents_hash);
    } else {
      fmt::print(stderr,
                 "Validation has failed to match with expected values, see the above errors for "
                 "specifics. This may be an error in the validation database!\n");
    }
    return {*error_code, std::nullopt};
  }

  return {ExtractorErrorCode::SUCCESS, meta_res};
}

std::optional<ISOMetadata> determineRelease(const std::filesystem::path& jak1_input_files) {
  std::optional<std::string> serial = std::nullopt;
  std::optional<xxh::hash64_t> elf_hash = std::nullopt;
  std::tie(serial, elf_hash) = findElfFile(jak1_input_files);

  if (!serial || !elf_hash) {
    return std::nullopt;
  }

  // Find the game in our tracking database
  auto dbEntry = isoDatabase.find(serial.value());
  if (dbEntry == isoDatabase.end()) {
    return std::nullopt;
  } else {
    auto& metaMap = dbEntry->second;
    auto meta_entry = metaMap.find(elf_hash.value());
    if (meta_entry == metaMap.end()) {
      return std::nullopt;
    } else {
      return std::make_optional(meta_entry->second);
    }
  }
}

void decompile(std::filesystem::path jak1_input_files) {
  using namespace decompiler;

  // Determine which config to use from the database
  auto meta = determineRelease(jak1_input_files);
  std::string decomp_config = "jak1_ntsc_black_label";
  if (meta.has_value()) {
    decomp_config = meta.value().decomp_config;
    fmt::print("INFO: Automatically detected decompiler config, using - {}\n", decomp_config);
  }

  Config config = read_config_file((file_util::get_jak_project_dir() / "decompiler" / "config" /
                                    fmt::format("{}.jsonc", decomp_config))
                                       .string(),
                                   {});

  std::vector<std::string> dgos, objs;

  // grab all DGOS we need (level + common)
  for (const auto& dgo_name : config.dgo_names) {
    std::string common_name = "GAME.CGO";
    if (dgo_name.length() > 3 && dgo_name.substr(dgo_name.length() - 3) == "DGO") {
      // ends in DGO, it's a level
      dgos.push_back((jak1_input_files / dgo_name).string());
    } else if (dgo_name.length() >= common_name.length() &&
               dgo_name.substr(dgo_name.length() - common_name.length()) == common_name) {
      // it's COMMON.CGO, we need that too.
      dgos.push_back((jak1_input_files / dgo_name).string());
    }
  }

  // grab all the object files we need (just text)
  for (const auto& obj_name : config.object_file_names) {
    if (obj_name.length() > 3 && obj_name.substr(obj_name.length() - 3) == "TXT") {
      // ends in TXT
      objs.push_back((jak1_input_files / obj_name).string());
    }
  }

  // set up objects
  ObjectFileDB db(dgos, config.obj_file_name_map_file, objs, {}, config);

  // save object files
  auto out_folder = (file_util::get_jak_project_dir() / "decompiler_out" / "jak1").string();
  auto raw_obj_folder = file_util::combine_path(out_folder, "raw_obj");
  file_util::create_dir_if_needed(raw_obj_folder);
  db.dump_raw_objects(raw_obj_folder);

  // analyze object file link data
  db.process_link_data(config);
  db.find_code(config);
  db.process_labels();

  // ensure asset dir exists
  file_util::create_dir_if_needed(file_util::get_file_path({"assets"}));

  // text files
  {
    auto result = db.process_game_text_files(config);
    if (!result.empty()) {
      file_util::write_text_file(file_util::get_file_path({"assets", "game_text.txt"}), result);
    }
  }

  // textures
  decompiler::TextureDB tex_db;
  file_util::write_text_file(file_util::get_file_path({"assets", "tpage-dir.txt"}),
                             db.process_tpages(tex_db));
  // texture replacements
  auto replacements_path = file_util::get_file_path({"texture_replacements"});
  if (std::filesystem::exists(replacements_path)) {
    tex_db.replace_textures(replacements_path);
  }

  // game count
  {
    auto result = db.process_game_count_file();
    if (!result.empty()) {
      file_util::write_text_file(file_util::get_file_path({"assets", "game_count.txt"}), result);
    }
  }

  // levels
  {
    extract_all_levels(db, tex_db, config.levels_to_extract, "GAME.CGO", config.hacks,
                       config.rip_levels, config.extract_collision);
  }
}

void compile(std::filesystem::path extracted_iso_path) {
  Compiler compiler;
  compiler.make_system().set_constant("*iso-data*", absolute(extracted_iso_path).string());
  compiler.make_system().set_constant("*use-iso-data-path*", true);

  auto buildinfo_path = (extracted_iso_path / "buildinfo.json").string();
  auto bi = parse_commented_json(file_util::read_text_file(buildinfo_path), buildinfo_path);
  auto all_flags = bi.at("flags").get<std::vector<std::string>>();
  int flags = 0;
  for (const auto& n : all_flags) {
    if (auto it = sGameIsoFlagNames.find(n); it != sGameIsoFlagNames.end()) {
      flags |= it->second;
    }
  }
  compiler.make_system().set_constant("*jak1-full-game*", !(flags & FLAG_JAK1_BLACK_LABEL));

  compiler.make_system().load_project_file(
      (file_util::get_jak_project_dir() / "goal_src" / "game.gp").string());
  compiler.run_front_end_on_string("(mi)");
}

void launch_game() {
  system(fmt::format("\"{}\"", (file_util::get_jak_project_dir() / "../gk").string()).c_str());
}

int main(int argc, char** argv) {
  std::filesystem::path data_dir_path;
  std::filesystem::path project_path_override;
  bool flag_runall = false;
  bool flag_extract = false;
  bool flag_fail_on_validation = false;
  bool flag_decompile = false;
  bool flag_compile = false;
  bool flag_play = false;
  bool flag_folder = false;

  lg::initialize();

  CLI::App app{"OpenGOAL Level Extraction Tool"};
  app.add_option("game-files-path", data_dir_path,
                 "The path to the folder with the ISO extracted or the ISO itself")
      ->check(CLI::ExistingPath)
      ->required();
  app.add_option("--proj-path", project_path_override,
                 "Explicitly set the location of the 'data/' folder")
      ->check(CLI::ExistingPath);
  app.add_flag("-a,--all", flag_runall, "Run all steps, from extraction to playing the game");
  app.add_flag("-e,--extract", flag_extract, "Extract the ISO");
  app.add_flag("-v,--validate", flag_fail_on_validation, "Fail on Validation Errors");
  app.add_flag("-d,--decompile", flag_decompile, "Decompile the game data");
  app.add_flag("-c,--compile", flag_compile, "Compile the game");
  app.add_flag("-p,--play", flag_play, "Play the game");
  app.add_flag("-f,--folder", flag_folder, "Extract from folder");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  fmt::print("Working Directory - {}\n", std::filesystem::current_path().string());

  // If no flag is set, we default to running everything
  if (!flag_extract && !flag_decompile && !flag_compile && !flag_play) {
    fmt::print("Running all steps, no flags provided!\n");
    flag_runall = true;
  }
  if (flag_runall) {
    flag_extract = true;
    flag_decompile = true;
    flag_compile = true;
    flag_play = true;
  }

  // todo: print revision here.
  if (!project_path_override.empty()) {
    setup_global_decompiler_stuff(std::make_optional(project_path_override));
  } else {
    setup_global_decompiler_stuff(std::nullopt);
  }

  std::filesystem::path path_to_iso_files = file_util::get_jak_project_dir() / "iso_data" / "_temp";

  // make sure the input looks right
  if (!std::filesystem::exists(data_dir_path)) {
    fmt::print("Error: input data path {} does not exist\n", data_dir_path.string());
    return 1;
  }

  if (flag_extract) {
    if (data_dir_path != path_to_iso_files) {
      // in case input is also output, don't just wipe everything (weird)
      std::filesystem::remove_all(path_to_iso_files);
    }
    std::filesystem::create_directories(path_to_iso_files);

    int flags = 0;
    if (std::filesystem::is_regular_file(data_dir_path)) {
      // it's a file, treat it as an ISO
      auto iso_file = extract_files(data_dir_path, path_to_iso_files);
      auto validation_res = validate(iso_file, path_to_iso_files);
      flags = validation_res.second->flags;
      if (validation_res.first == ExtractorErrorCode::VALIDATION_BAD_EXTRACTION) {
        // We fail here regardless of the flag
        return static_cast<int>(validation_res.first);
      } else if (flag_fail_on_validation && validation_res.first != ExtractorErrorCode::SUCCESS) {
        return static_cast<int>(validation_res.first);
      }
    } else if (std::filesystem::is_directory(data_dir_path)) {
      if (!flag_folder) {
        // if we didn't request a folder explicitly, but we got one, assume something went wrong.
        fmt::print("Error: got a folder, but didn't get folder flag\n");
        return static_cast<int>(ExtractorErrorCode::VALIDATION_BAD_ISO_CONTENTS);
      }
      path_to_iso_files = data_dir_path;
    }

    // write out a json file with some metadata for the game
    nlohmann::json buildinfo_json;
    auto flags_json = nlohmann::json::array();
    for (const auto& [n, f] : sGameIsoFlagNames) {
      if (flags & f) {
        flags_json.push_back(n);
      }
    }
    buildinfo_json["flags"] = flags_json;
    // something tells me a ps2 game is unlikely to have a json file in root
    file_util::write_text_file((path_to_iso_files / "buildinfo.json").string(),
                               buildinfo_json.dump(2));
  }

  if (flag_decompile) {
    try {
      decompile(path_to_iso_files);
    } catch (std::exception& e) {
      lg::error("Error during decompile: {}", e.what());
      return static_cast<int>(ExtractorErrorCode::DECOMPILATION_GENERIC_ERROR);
    }
  }

  if (flag_compile) {
    compile(path_to_iso_files);
  }

  if (flag_play) {
    launch_game();
  }

  return 0;
}

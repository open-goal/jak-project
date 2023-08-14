#include <map>
#include <regex>
#include <unordered_map>

#include "extractor_util.hpp"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/read_iso_file.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/config.h"
#include "decompiler/level_extractor/extract_level.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"

IsoFile extract_files(fs::path input_file_path, fs::path extracted_iso_path) {
  lg::info(
      "Note: Provided game data path '{}' points to a file, not a directory. Assuming it's an ISO "
      "file and attempting to extract!",
      input_file_path.string());

  fs::create_directories(extracted_iso_path);

  auto fp = file_util::open_file(input_file_path, "rb");
  ASSERT_MSG(fp, "failed to open input ISO file");
  IsoFile iso = unpack_iso_files(fp, extracted_iso_path, true, true);
  fclose(fp);
  return iso;
}

std::tuple<std::optional<ISOMetadata>, ExtractorErrorCode> validate(
    const fs::path& extracted_iso_path,
    const uint64_t expected_hash,
    const int expected_num_files) {
  if (!fs::exists(extracted_iso_path / "DGO")) {
    lg::error("input folder doesn't have a DGO folder. Is this the right input?");
    return {std::nullopt, ExtractorErrorCode::VALIDATION_BAD_EXTRACTION};
  }

  const auto [serial, elf_hash] = findElfFile(extracted_iso_path);

  if (!serial || !elf_hash) {
    lg::error("Unable to locate a Serial/ELF file!");
    // No point in continuing here
    return {std::nullopt, ExtractorErrorCode::VALIDATION_CANT_LOCATE_ELF};
  }

  // Find the game in our tracking database
  auto dbEntry = isoDatabase.find(serial.value());
  if (dbEntry == isoDatabase.end()) {
    lg::error("Serial '{}' not found in the validation database", serial.value());
    log_potential_new_db_entry(ExtractorErrorCode::VALIDATION_SERIAL_MISSING_FROM_DB,
                               serial.value(), elf_hash.value(), expected_num_files, expected_hash);
    return {std::nullopt, ExtractorErrorCode::VALIDATION_SERIAL_MISSING_FROM_DB};
  }

  auto& metaMap = dbEntry->second;
  auto meta_entry = metaMap.find(elf_hash.value());
  if (meta_entry == metaMap.end()) {
    lg::error(
        "ELF Hash '{}' not found in the validation database, is this a new or "
        "modified version of the same game?",
        elf_hash.value());
    log_potential_new_db_entry(ExtractorErrorCode::VALIDATION_ELF_MISSING_FROM_DB, serial.value(),
                               elf_hash.value(), expected_num_files, expected_hash);
    return {std::nullopt, ExtractorErrorCode::VALIDATION_ELF_MISSING_FROM_DB};
  }

  auto& version_info = meta_entry->second;
  // Print out some information
  lg::info("Detected Game Metadata:");
  lg::info("\tDetected - {}", version_info.canonical_name);
  lg::info("\tRegion - {}", get_territory_name(version_info.region));
  lg::info("\tSerial - {}", dbEntry->first);
  lg::info("\tUses Decompiler Config Version - {}", version_info.decomp_config_version);

  // - Number of Files
  if (version_info.num_files != expected_num_files) {
    lg::error("Extracted an unexpected number of files. Expected '{}', Actual '{}'",
              version_info.num_files, expected_num_files);
    return {std::nullopt, ExtractorErrorCode::VALIDATION_INCORRECT_EXTRACTION_COUNT};
  }
  // Check the ISO Hash
  if (version_info.contents_hash != expected_hash) {
    lg::error("Overall ISO content's hash does not match. Expected '{}', Actual '{}'",
              version_info.contents_hash, expected_hash);
    return {std::nullopt, ExtractorErrorCode::VALIDATION_FILE_CONTENTS_UNEXPECTED};
  }

  return {
      std::make_optional(version_info),
      ExtractorErrorCode::SUCCESS,
  };
}

void decompile(const fs::path& iso_data_path, const std::string& data_subfolder) {
  using namespace decompiler;

  // Determine which config to use from the database
  const auto version_info = get_version_info_or_default(iso_data_path);

  Config config = read_config_file(file_util::get_jak_project_dir() / "decompiler" / "config" /
                                       version_info.game_name /
                                       fmt::format("{}_config.jsonc", version_info.game_name),
                                   version_info.decomp_config_version);

  std::vector<fs::path> dgos, objs, tex_strs;

  // grab all DGOS we need (level + common)
  // TODO - Jak 2 - jak 1 specific code?
  for (const auto& dgo_name : config.dgo_names) {
    std::string common_name = "GAME.CGO";
    if (dgo_name.length() > 3 && dgo_name.substr(dgo_name.length() - 3) == "DGO") {
      // ends in DGO, it's a level
      dgos.push_back(iso_data_path / dgo_name);
    } else if (dgo_name.length() >= common_name.length() &&
               dgo_name.substr(dgo_name.length() - common_name.length()) == common_name) {
      // it's COMMON.CGO, we need that too.
      dgos.push_back(iso_data_path / dgo_name);
    }
  }

  // grab all the object files we need (just text)
  for (const auto& obj_name : config.object_file_names) {
    if (obj_name.length() > 3 && obj_name.substr(obj_name.length() - 3) == "TXT") {
      // ends in TXT
      objs.push_back(iso_data_path / obj_name);
    }
  }

  for (const auto& str_name : config.str_texture_file_names) {
    tex_strs.push_back(iso_data_path / str_name);
  }

  // set up objects
  ObjectFileDB db(dgos, fs::path(config.obj_file_name_map_file), objs, {}, tex_strs, config);

  // save object files
  auto out_folder = file_util::get_jak_project_dir() / "decompiler_out" / data_subfolder;
  auto raw_obj_folder = out_folder / "raw_obj";
  file_util::create_dir_if_needed(raw_obj_folder);
  db.dump_raw_objects(raw_obj_folder);

  // analyze object file link data
  db.process_link_data(config);
  db.find_code(config);
  db.process_labels();

  // ensure asset dir exists
  file_util::create_dir_if_needed(out_folder / "assets");

  // text files
  {
    auto result = db.process_game_text_files(config);
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_text.txt", result);
    }
  }

  // textures
  decompiler::TextureDB tex_db;
  auto textures_out = out_folder / "textures";
  file_util::create_dir_if_needed(textures_out);
  file_util::write_text_file(textures_out / "tpage-dir.txt",
                             db.process_tpages(tex_db, textures_out, config));
  // texture replacements
  auto replacements_path = file_util::get_jak_project_dir() / "texture_replacements";
  if (fs::exists(replacements_path)) {
    tex_db.replace_textures(replacements_path);
  }

  // game count
  {
    auto result = db.process_game_count_file();
    if (!result.empty()) {
      file_util::write_text_file(out_folder / "assets" / "game_count.txt", result);
    }
  }

  // levels
  {
    auto level_out_path =
        file_util::get_jak_project_dir() / "out" / game_version_names[config.game_version] / "fr3";
    file_util::create_dir_if_needed(level_out_path);
    extract_all_levels(db, tex_db, config.levels_to_extract, "GAME.CGO", config, config.rip_levels,
                       config.extract_collision, level_out_path);
  }
}

ExtractorErrorCode compile(const fs::path& iso_data_path, const std::string& data_subfolder) {
  // Determine which config to use from the database
  const auto version_info = get_version_info_or_default(iso_data_path);

  Compiler compiler(game_name_to_version(version_info.game_name));
  compiler.make_system().set_constant("*iso-data*", absolute(iso_data_path).string());
  compiler.make_system().set_constant("*use-iso-data-path*", true);

  int flags = 0;
  for (const auto& flag : version_info.flags) {
    if (auto it = sGameIsoFlagNames.find(flag); it != sGameIsoFlagNames.end()) {
      flags |= it->second;
    }
  }

  if (version_info.game_name == "jak1") {
    compiler.make_system().set_constant("*jak1-full-game*", !(flags & FLAG_JAK1_BLACK_LABEL));
    compiler.make_system().set_constant("*jak1-territory*", version_info.region);
  }

  auto project_path = file_util::get_jak_project_dir() / "goal_src" / data_subfolder / "game.gp";
  if (!fs::exists(project_path)) {
    return ExtractorErrorCode::COMPILATION_BAD_PROJECT_PATH;
  }

  compiler.make_system().load_project_file(project_path.string());
  compiler.run_front_end_on_string("(mi)");

  return ExtractorErrorCode::SUCCESS;
}

void launch_game() {
  system(fmt::format("\"{}\"", (file_util::get_jak_project_dir() / "../gk").string()).c_str());
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path input_file_path;
  fs::path project_path_override;
  bool flag_runall = false;
  bool flag_extract = false;
  bool flag_fail_on_validation = false;
  bool flag_decompile = false;
  bool flag_compile = false;
  bool flag_play = false;
  bool flag_folder = false;
  std::string game_name = "jak1";

  lg::initialize();

  CLI::App app{"OpenGOAL Level Extraction Tool"};
  app.add_option("game-files-path", input_file_path,
                 "The path to the folder with the ISO extracted or the ISO itself")
      ->required();
  app.add_option("--proj-path", project_path_override,
                 "Explicitly set the location of the 'data/' folder");
  app.add_flag("-g,--game", game_name, "Specify the game name, defaults to 'jak1'");
  app.add_flag("-a,--all", flag_runall, "Run all steps, from extraction to playing the game");
  app.add_flag("-e,--extract", flag_extract, "Extract the ISO");
  app.add_flag("-v,--validate", flag_fail_on_validation,
               "Fail on validation errors during extraction");
  app.add_flag("-d,--decompile", flag_decompile, "Decompile the game data");
  app.add_flag("-c,--compile", flag_compile, "Compile the game");
  app.add_flag("-p,--play", flag_play, "Play the game");
  app.add_flag("-f,--folder", flag_folder, "Extract from folder");
  define_common_cli_arguments(app);
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  lg::info("Working Directory - {}", fs::current_path().string());

  // If no flag is set, we default to running everything
  if (!flag_extract && !flag_decompile && !flag_compile && !flag_play) {
    lg::info("Running all steps, no flags provided!");
    flag_runall = true;
  }
  if (flag_runall) {
    flag_extract = true;
    flag_decompile = true;
    flag_compile = true;
    flag_play = true;
  }

  // - SETUP
  decompiler::init_opcode_info();
  if (!project_path_override.empty()) {
    if (!fs::exists(project_path_override)) {
      lg::error("Error: project path override '{}' does not exist", project_path_override.string());
      return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
    }
    auto ok = file_util::setup_project_path(project_path_override);
    if (!ok) {
      lg::error("Could not setup project path!");
      return 1;
    }
  } else {
    auto ok = file_util::setup_project_path({});
    if (!ok) {
      lg::error("Could not setup project path!");
      return 1;
    }
  }

  try {
    lg::set_file("extractor");
    if (_cli_flag_disable_ansi) {
      lg::disable_ansi_colors();
    }
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  fs::path iso_data_path;

  // - INPUT VALIDATION
  if (!fs::exists(input_file_path)) {
    lg::error("Error: input game file path '{}' does not exist", input_file_path.string());
    return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
  }
  if (data_subfolders.count(game_name) == 0) {
    lg::error("Error: input game name '{}' is not valid", game_name);
    return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
  }
  std::string data_subfolder = data_subfolders[game_name];

  if (flag_extract) {
    // we extract to a temporary location because we don't know what we're extracting yet!
    fs::path temp_iso_extract_location = file_util::get_jak_project_dir() / "iso_data" / "_temp";
    if (input_file_path != temp_iso_extract_location) {
      // in case input is also output, don't just wipe everything (weird)
      fs::remove_all(temp_iso_extract_location);
    }
    fs::create_directories(temp_iso_extract_location);

    if (fs::is_regular_file(input_file_path)) {
      // If it's a file, then it better be an iso file
      const auto [iso_ok, iso_code] = is_iso_file(input_file_path);
      if (!iso_ok) {
        return static_cast<int>(iso_code);
      }

      // Extract to the temporary location
      const auto iso_file = extract_files(input_file_path, temp_iso_extract_location);
      // Get hash and file count
      const auto [hash, file_count] = calculate_extraction_hash(iso_file);
      // Validate the result to determine the release
      const auto [version_info, validate_code] =
          validate(temp_iso_extract_location, hash, file_count);
      if (validate_code == ExtractorErrorCode::VALIDATION_BAD_EXTRACTION ||
          (flag_fail_on_validation && validate_code != ExtractorErrorCode::SUCCESS)) {
        return static_cast<int>(validate_code);
      }
      // Finalize the folder name now that we know where it should go
      if (!version_info) {
        lg::error("could not verify release, so not finalizing iso_data, leaving in '_temp'");
        iso_data_path = temp_iso_extract_location;
      } else {
        // We know the version since we just extracted it, so the user didn't need to provide this
        // explicitly
        data_subfolder = data_subfolders[version_info->game_name];
        iso_data_path = file_util::get_jak_project_dir() / "iso_data" / data_subfolder;
        if (fs::exists(iso_data_path)) {
          fs::remove_all(iso_data_path);
        }

        // std::filesystem doesn't have a rename for dirs...
        fs::copy(temp_iso_extract_location, iso_data_path, fs::copy_options::recursive);
        fs::remove_all(temp_iso_extract_location);
      }
    } else if (fs::is_directory(input_file_path)) {
      if (!flag_folder) {
        // if we didn't request a folder explicitly, but we got one, assume something went wrong.
        lg::error("got a folder, but didn't get folder flag");
        return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
      }
      iso_data_path = input_file_path;
      // Get hash and file count
      const auto [hash, file_count] = calculate_extraction_hash(iso_data_path);
      // Validate
      auto [version_info, code] = validate(iso_data_path, hash, file_count);
    }

    // write out a json file with some metadata for the game
    if (fs::exists(iso_data_path / "buildinfo.json")) {
      fs::remove(iso_data_path / "buildinfo.json");
    }
    const auto [serial, elf_hash] = findElfFile(iso_data_path);
    BuildInfo build_info;
    if (serial.has_value()) {
      build_info.serial = serial.value();
    }
    if (elf_hash.has_value()) {
      build_info.elf_hash = elf_hash.value();
    }
    const nlohmann::json json_data{build_info};
    file_util::write_text_file((iso_data_path / "buildinfo.json").string(), json_data.dump(2));
  } else {
    // If we did not extract, we have no clue what game the user is trying to decompile / compile
    // this is why the user has to specify this!
    iso_data_path = file_util::get_jak_project_dir() / "iso_data" / data_subfolder;
  }

  if (flag_decompile) {
    try {
      decompile(iso_data_path, data_subfolder);
    } catch (std::exception& e) {
      lg::error("Error during decompile: {}", e.what());
      return static_cast<int>(ExtractorErrorCode::DECOMPILATION_GENERIC_ERROR);
    }
  }

  if (flag_compile) {
    compile(iso_data_path, data_subfolder);
  }

  if (flag_play) {
    launch_game();
  }

  return 0;
}

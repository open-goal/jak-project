#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/read_iso_file.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/config.h"
#include "decompiler/decompilation_process.h"
#include "decompiler/extractor/extractor_util.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"

// used for - decompiler_out/<jak1> and iso_data/<jak1>
const std::unordered_map<std::string, std::string> data_subfolders = {{"jak1", "jak1"},
                                                                      {"jak2", "jak2"}};

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
  const auto& iso_database = extractor_iso_database();
  auto dbEntry = iso_database.find(serial.value());
  if (dbEntry == iso_database.end()) {
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
  if (version_info.contents_hash.count(expected_hash) == 0) {
    std::string all_expected;
    for (const auto& hash : version_info.contents_hash) {
      all_expected += fmt::format("{}, ", hash);
    }
    lg::error("Overall ISO content's hash does not match. Expected '{}', Actual '{}'", all_expected,
              expected_hash);
    return {std::make_optional(version_info),
            ExtractorErrorCode::VALIDATION_FILE_CONTENTS_UNEXPECTED};
  }

  return {
      std::make_optional(version_info),
      ExtractorErrorCode::SUCCESS,
  };
}

ExtractorErrorCode decompile(const fs::path& in_folder,
                             const std::string& data_subfolder,
                             const std::string& config_override) {
  // Determine which config to use from the database
  const auto version_info = get_version_info_or_default(in_folder);

  decompiler::Config config = decompiler::read_config_file(
      file_util::get_jak_project_dir() / "decompiler" / "config" / version_info.game_name /
          fmt::format("{}_config.jsonc", version_info.game_name),
      version_info.decomp_config_version, config_override);

  auto out_folder = file_util::get_jak_project_dir() / "decompiler_out" / data_subfolder;

  const auto result = run_decompilation_process(config, in_folder, out_folder, true);
  if (result != 0) {
    return ExtractorErrorCode::DECOMPILATION_GENERIC_ERROR;
  }
  return ExtractorErrorCode::SUCCESS;
}

const std::unordered_map<std::string, GameIsoFlags> game_iso_flag_names = {
    {"jak1-black-label", FLAG_JAK1_BLACK_LABEL}};

ExtractorErrorCode compile(const fs::path& iso_data_path, const std::string& data_subfolder) {
  // Determine which config to use from the database
  const auto version_info = get_version_info_or_default(iso_data_path);

  Compiler compiler(game_name_to_version(version_info.game_name));
  compiler.make_system().set_constant("*iso-data*", absolute(iso_data_path).string());
  compiler.make_system().set_constant("*use-iso-data-path*", true);
  file_util::set_iso_data_dir(absolute(iso_data_path));
  lg::info("set iso_data_dir to {}", absolute(iso_data_path).string());

  int flags = 0;
  for (const auto& flag : version_info.flags) {
    if (auto it = game_iso_flag_names.find(flag); it != game_iso_flag_names.end()) {
      flags |= it->second;
    }
  }

  compiler.get_goos().set_global_variable_to_int("*default-territory*", version_info.region);
  if (version_info.game_name == "jak1") {
    compiler.make_system().set_constant("*jak1-full-game*", !(flags & FLAG_JAK1_BLACK_LABEL));
    compiler.get_goos().set_global_variable_to_symbol(
        "*jak1-full-game*", !(flags & FLAG_JAK1_BLACK_LABEL) ? "#t" : "#f");
  }

  auto project_path = file_util::get_jak_project_dir() / "goal_src" / data_subfolder / "game.gp";
  if (!fs::exists(project_path)) {
    return ExtractorErrorCode::COMPILATION_BAD_PROJECT_PATH;
  }

  compiler.make_system().load_project_file(project_path.string());
  compiler.run_front_end_on_string("(mi)");

  return ExtractorErrorCode::SUCCESS;
}

void launch_game(const std::string& game_version) {
  system(fmt::format("\"{}\" -g {}", (file_util::get_jak_project_dir() / "../gk").string(),
                     game_version)
             .c_str());
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path input_file_path;
  fs::path project_path_override;
  fs::path extraction_path;
  bool flag_runall = false;
  bool flag_extract = false;
  bool flag_fail_on_validation = false;
  bool flag_decompile = false;
  bool flag_compile = false;
  bool flag_play = false;
  bool flag_folder = false;
  std::string game_name = "jak1";
  std::string decomp_config_override = "{}";

  lg::initialize();

  CLI::App app{"OpenGOAL Extractor (ISO Tools + Decompiler + Compiler)"};
  app.add_option("game-files-path", input_file_path,
                 "The path to the folder with the ISO extracted or the ISO itself")
      ->required();
  app.add_option("--proj-path", project_path_override,
                 "Explicitly set the location of the 'data/' folder");
  app.add_option("--extract-path", extraction_path,
                 "Explicitly set the location for where the ISO should be extracted");
  app.add_option("-g,--game", game_name, "Specify the game name, defaults to 'jak1'");
  app.add_option(
      "--decomp-config-override", decomp_config_override,
      "JSON provided will be merged with the decompiler config, use to override options");
  app.add_flag("-a,--all", flag_runall, "Run all steps, from extraction to playing the game");
  app.add_flag("-e,--extract", flag_extract, "Extract the ISO");
  app.add_flag("-v,--validate", flag_fail_on_validation,
               "Fail on validation errors during extraction");
  app.add_flag("-d,--decompile", flag_decompile, "Decompile the game data");
  app.add_flag("-c,--compile", flag_compile, "Compile the game");
  app.add_flag("-p,--play", flag_play, "Play the game");
  app.add_flag("-f,--folder", flag_folder, "Take ISO input from a folder");
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
  if (!project_path_override.empty()) {
    if (!fs::exists(project_path_override)) {
      lg::error("Error: project path override '{}' does not exist", project_path_override.string());
      return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT_MISSING_FOLDER);
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
    return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT_MISSING_FOLDER);
  }
  if (data_subfolders.count(game_name) == 0) {
    lg::error("Error: input game name '{}' is not valid", game_name);
    return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
  }
  std::string data_subfolder = data_subfolders.at(game_name);

  if (flag_extract) {
    // we extract to a temporary location because we don't know what we're extracting yet!
    fs::path temp_iso_extract_location = file_util::get_jak_project_dir() / "iso_data" / "_temp";
    if (!extraction_path.empty()) {
      temp_iso_extract_location = extraction_path / "_temp";
    }
    lg::info("Extracting ISO to temporary dir at: {}", temp_iso_extract_location.string());
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
        data_subfolder = data_subfolders.at(version_info->game_name);
        game_name = version_info->game_name;
        if (!extraction_path.empty()) {
          iso_data_path = extraction_path / data_subfolder;
        } else {
          iso_data_path = file_util::get_jak_project_dir() / "iso_data" / data_subfolder;
        }
        if (fs::exists(iso_data_path) && iso_data_path != temp_iso_extract_location) {
          fs::remove_all(iso_data_path);
        }

        // std::filesystem doesn't have a rename for dirs...
        // NOTE - potential disaster here, don't do either if the directories are the same location
        // or don't copy if the temp location is _inside_ the destination directory
        if (!file_util::is_dir_in_dir(iso_data_path, temp_iso_extract_location)) {
          fs::copy(temp_iso_extract_location, iso_data_path, fs::copy_options::recursive);
        }
        if (iso_data_path != temp_iso_extract_location) {
          // in case input is also output, don't just wipe everything (weird)
          fs::remove_all(temp_iso_extract_location);
        }
      }
    } else if (fs::is_directory(input_file_path)) {
      if (!flag_folder) {
        // if we didn't request a folder explicitly, but we got one, assume something went wrong.
        lg::error("got a folder, but didn't provide the folder flag");
        return static_cast<int>(ExtractorErrorCode::INVALID_CLI_INPUT);
      }
      iso_data_path = input_file_path;
      // Get hash and file count
      const auto [hash, file_count] = calculate_extraction_hash(iso_data_path);
      // Validate
      auto [version_info, validate_code] = validate(iso_data_path, hash, file_count);
      if (validate_code == ExtractorErrorCode::VALIDATION_BAD_EXTRACTION ||
          (flag_fail_on_validation && validate_code != ExtractorErrorCode::SUCCESS)) {
        return static_cast<int>(validate_code);
      }
      data_subfolder = data_subfolders.at(version_info->game_name);
      game_name = version_info->game_name;
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
    if (flag_folder) {
      iso_data_path = input_file_path;
    } else {
      iso_data_path = file_util::get_jak_project_dir() / "iso_data" / data_subfolder;
    }
  }

  if (flag_decompile) {
    try {
      const auto status_code = decompile(iso_data_path, data_subfolder, decomp_config_override);
      if (status_code != ExtractorErrorCode::SUCCESS) {
        return static_cast<int>(status_code);
      }
    } catch (std::exception& e) {
      lg::error("Error during decompile: {}", e.what());
      return static_cast<int>(ExtractorErrorCode::DECOMPILATION_GENERIC_ERROR);
    }
  }

  if (flag_compile) {
    const auto status_code = compile(iso_data_path, data_subfolder);
    if (status_code != ExtractorErrorCode::SUCCESS) {
      return static_cast<int>(status_code);
    }
  }

  if (flag_play) {
    launch_game(game_name);
  }

  return 0;
}

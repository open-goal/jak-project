#pragma once

#include <optional>
#include <set>
#include <unordered_map>

#include "common/util/FileUtil.h"
#include "common/util/read_iso_file.h"

#include "third-party/json.hpp"

enum class ExtractorErrorCode {
  SUCCESS = 0,
  INVALID_CLI_INPUT = 3990,
  VALIDATION_CANT_LOCATE_ELF = 4000,
  VALIDATION_SERIAL_MISSING_FROM_DB = 4001,
  VALIDATION_ELF_MISSING_FROM_DB = 4002,
  VALIDATION_BAD_ISO_CONTENTS = 4010,
  VALIDATION_INCORRECT_EXTRACTION_COUNT = 4011,
  VALIDATION_FILE_CONTENTS_UNEXPECTED = 4012,
  VALIDATION_BAD_EXTRACTION = 4020,
  DECOMPILATION_GENERIC_ERROR = 4030,
  EXTRACTION_INVALID_ISO_PATH = 4040,
  EXTRACTION_ISO_UNEXPECTED_SIZE = 4041,
  COMPILATION_BAD_PROJECT_PATH = 4050,
};

enum GameIsoFlags { FLAG_JAK1_BLACK_LABEL = (1 << 0) };

std::string get_territory_name(int territory);

struct ISOMetadata {
  std::string canonical_name;
  int region;  // territory code
  int num_files;
  std::set<uint64_t> contents_hash;
  std::string decomp_config_version;
  std::string game_name;
  std::vector<std::string> flags;
};

// { SERIAL : { ELF_HASH : ISOMetadataDatabase } }
const std::unordered_map<std::string, std::unordered_map<uint64_t, ISOMetadata>>&
extractor_iso_database();

// This is all we need to re-fetch info from the database
// - if this changes such that we have a collision in the future,
//   then the database isn't adequate and everything must change
struct BuildInfo {
  std::string serial = "";
  uint64_t elf_hash = 0;
};
void to_json(nlohmann::json& j, const BuildInfo& info);
void from_json(const nlohmann::json& j, BuildInfo& info);

std::optional<BuildInfo> get_buildinfo_from_path(fs::path iso_data_path);

std::optional<ISOMetadata> get_version_info_from_build_info(const BuildInfo& build_info);

ISOMetadata get_version_info_or_default(const fs::path& iso_data_path);

std::tuple<std::optional<std::string>, std::optional<uint64_t>> findElfFile(
    const fs::path& extracted_iso_path);

void log_potential_new_db_entry(ExtractorErrorCode error_code,
                                const std::string& serial,
                                const uint64_t elf_hash,
                                const int files_extracted,
                                const uint64_t contents_hash);

std::tuple<bool, ExtractorErrorCode> is_iso_file(fs::path path_to_supposed_iso);

std::tuple<uint64_t, int> calculate_extraction_hash(const IsoFile& iso_file);

std::tuple<uint64_t, int> calculate_extraction_hash(const fs::path& extracted_iso_path);

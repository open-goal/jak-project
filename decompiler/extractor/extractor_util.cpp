#include "extractor_util.h"

#include <optional>
#include <regex>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/read_iso_file.h"

#include "game/kernel/common/kboot.h"

#include "third-party/json.hpp"
#include "third-party/zstd/lib/common/xxhash.h"

const std::unordered_map<int, std::string> game_iso_territory_map = {
    {GAME_TERRITORY_SCEA, "NTSC-U"},
    {GAME_TERRITORY_SCEE, "PAL"},
    {GAME_TERRITORY_SCEI, "NTSC-J"},
    {GAME_TERRITORY_SCEK, "NTSC-K"}};

std::string get_territory_name(int territory) {
  ASSERT_MSG(game_iso_territory_map.count(territory),
             fmt::format("territory {} not found in territory name map"));
  return game_iso_territory_map.at(territory);
}

const ISOMetadata jak1_ntsc_black_label_info = {"Jak & Daxter™: The Precursor Legacy (Black Label)",
                                                GAME_TERRITORY_SCEA,
                                                337,
                                                {11363853835861842434U},
                                                "ntsc_v1",
                                                "jak1",
                                                {"jak1-black-label"}};

// { SERIAL : { ELF_HASH : ISOMetadataDatabase } }
const std::unordered_map<std::string, std::unordered_map<uint64_t, ISOMetadata>>&
extractor_iso_database() {
  static const std::unordered_map<std::string, std::unordered_map<uint64_t, ISOMetadata>> database =
      {
          {"SCUS-97124",
           {{7280758013604870207U, jak1_ntsc_black_label_info},
            {744661860962747854,
             {"Jak & Daxter™: The Precursor Legacy",
              GAME_TERRITORY_SCEA,
              338,
              {8538304367812415885U},
              "ntsc_v2",
              "jak1",
              {}}}}},
          {"SCES-50361",
           {{12150718117852276522U,
             {"Jak & Daxter™: The Precursor Legacy",
              GAME_TERRITORY_SCEE,
              338,
              {16850370297611763875U},
              "pal",
              "jak1",
              {}}}}},
          {"SCPS-15021",
           {{16909372048085114219U,
             {"ジャックＸダクスター　～　旧世界の遺産",
              GAME_TERRITORY_SCEI,
              338,
              {1262350561338887717U},
              "jp",
              "jak1",
              {}}}}},
          {"SCPS-56003",
           {{7280758013604870207U,
             {"Jak & Daxter: 구세계의 유산",
              GAME_TERRITORY_SCEA,
              338,
              {13924540661438229398U},
              "ntsc_v1",
              "jak1",
              {}}}}},
          // Jak 2, NTSC-U v1 and v2.
          // we put both of them together because they have the same serial and ELF.
          {"SCUS-97265",             // serial from ELF name
           {{18445016742498932084U,  // hash of ELF
             {"Jak II",              // canonical name
              GAME_TERRITORY_SCEA,
              593,                                           // number of files
              {4835330407820245819U, 5223305410190549348U},  // iso hash
              "ntsc_v1",                                     // decompiler config
              "jak2",
              {}}}}},
          // Jak 2 PAL
          {"SCES-51608",             // serial from ELF name
           {{18188891052467821088U,  // hash of ELF
             {"Jak II: Renegade",    // canonical name
              GAME_TERRITORY_SCEE,
              593,                     // number of files
              {8410801891219727031U},  // iso hash
              "pal",                   // decompiler config
              "jak2",
              {}}}}},
          // Jak 2 NTSC-J
          {"SCPS-15057",                // serial from ELF name
           {{7409991384254810731U,      // hash of ELF
             {"ジャックＸダクスター2",  // canonical name
              GAME_TERRITORY_SCEI,
              593,                     // number of files
              {1686904681401593185U},  // iso hash
              "jp",                    // decompiler config
              "jak2",
              {}}}}},
          // Jak 2 NTSC-K
          {"SCKA-20010",            // serial from ELF name
           {{8398029689314218575U,  // hash of ELF
             {"Jak II",             // canonical name
              GAME_TERRITORY_SCEI,
              593,                     // number of files
              {4637199624374114440U},  // iso hash
              "ko",                    // decompiler config
              "jak2",
              {}}}}},
      };
  return database;
}

void to_json(nlohmann::json& j, const BuildInfo& info) {
  j = nlohmann::json{{"serial", info.serial}, {"elf_hash", info.elf_hash}};
}

void from_json(const nlohmann::json& j, BuildInfo& info) {
  j[0].at("serial").get_to(info.serial);
  j[0].at("elf_hash").get_to(info.elf_hash);
}

std::optional<BuildInfo> get_buildinfo_from_path(fs::path iso_data_path) {
  if (!fs::exists(iso_data_path / "buildinfo.json")) {
    return {};
  }
  auto buildinfo_path = (iso_data_path / "buildinfo.json").string();
  try {
    const auto buildinfo_data = file_util::read_text_file(buildinfo_path);
    lg::info("Found version info - {}", buildinfo_data);
    return parse_commented_json(buildinfo_data, buildinfo_path).get<BuildInfo>();
  } catch (std::exception& e) {
    lg::error("JSON parsing error on buildinfo.json - {}", e.what());
    return {};
  }
}

std::optional<ISOMetadata> get_version_info_from_build_info(const BuildInfo& build_info) {
  if (build_info.serial.empty() || build_info.elf_hash == 0) {
    return {};
  }
  auto dbEntry = extractor_iso_database().find(build_info.serial);
  if (dbEntry == extractor_iso_database().end()) {
    return {};
  }

  auto& metaMap = dbEntry->second;
  auto meta_entry = metaMap.find(build_info.elf_hash);
  if (meta_entry == metaMap.end()) {
    return {};
  }
  return std::make_optional(meta_entry->second);
}

ISOMetadata get_version_info_or_default(const fs::path& iso_data_path) {
  ISOMetadata version_info = jak1_ntsc_black_label_info;
  const auto build_info = get_buildinfo_from_path(iso_data_path);
  if (!build_info) {
    lg::warn(
        "unable to locate buildinfo.json file in iso data path, defaulting to Jak 1 - NTSC-U Black "
        "Label");
  } else {
    auto maybe_version_info = get_version_info_from_build_info(build_info.value());
    if (!maybe_version_info) {
      lg::warn(
          "unable to determine game version from buildinfo.json file, defaulting to Jak 1 - NTSC-U "
          "Black Label");
    } else {
      version_info = maybe_version_info.value();
    }
  }
  return version_info;
}

std::tuple<std::optional<std::string>, std::optional<uint64_t>> findElfFile(
    const fs::path& extracted_iso_path) {
  std::optional<std::string> serial = std::nullopt;
  std::optional<uint64_t> elf_hash = std::nullopt;
  for (const auto& entry : fs::directory_iterator(extracted_iso_path)) {
    auto as_str = entry.path().filename().string();
    if (std::regex_match(as_str, std::regex(".{4}_.{3}\\..{2}"))) {
      serial = std::make_optional(
          fmt::format("{}-{}", as_str.substr(0, 4), as_str.substr(5, 3) + as_str.substr(9, 2)));
      // We already found the path, so hash it while we're here
      auto fp = file_util::open_file(entry.path().string().c_str(), "rb");
      fseek(fp, 0, SEEK_END);
      size_t size = ftell(fp);
      std::vector<u8> buffer(size);
      rewind(fp);
      fread(&buffer[0], sizeof(std::vector<u8>::value_type), buffer.size(), fp);
      elf_hash = std::make_optional(XXH64(buffer.data(), buffer.size(), 0));
      fclose(fp);
      break;
    }
  }
  return {serial, elf_hash};
}

void log_potential_new_db_entry(ExtractorErrorCode error_code,
                                const std::string& serial,
                                const uint64_t elf_hash,
                                const int files_extracted,
                                const uint64_t contents_hash) {
  // Finally, return the result
  // Generate the map entry to make things simple, just convienance
  if (error_code == ExtractorErrorCode::VALIDATION_SERIAL_MISSING_FROM_DB) {
    lg::info(
        "If this is a new release or version that should be supported, consider adding the "
        "following serial entry to the database:");
    lg::info("serial {}, elf hash {}, files {}, hash {}", serial, elf_hash, files_extracted,
             contents_hash);
  } else if (error_code == ExtractorErrorCode::VALIDATION_ELF_MISSING_FROM_DB) {
    lg::info(
        "If this is a new release or version that should be supported, consider adding the "
        "following ELF entry to the database under the '{}' serial:",
        serial);
    lg::info(
        "\t'{{{}, {{\"GAME_TITLE\", \"NTSC-U/PAL/NTSC-J\", {}, {}U, "
        "\"DECOMP_CONFIF_FILENAME_NO_EXTENSION\", \"jak1|jak2|jak3|jakx\", {}}}}}'",
        elf_hash, files_extracted, contents_hash);
  }
}

std::tuple<bool, ExtractorErrorCode> is_iso_file(fs::path path_to_supposed_iso) {
  // it's a file, normalize extension case and verify it's an ISO file
  std::string ext = path_to_supposed_iso.extension().string();
  if (!std::regex_match(ext, std::regex("\\.(iso|ISO)"))) {
    lg::error("Provided game data path contains a file that isn't a .ISO!");
    return {false, ExtractorErrorCode::EXTRACTION_INVALID_ISO_PATH};
  }

  // make sure the .iso is greater than 1GB in size
  // to-do: verify game header data as well
  if (fs::file_size(path_to_supposed_iso) < 1000000000) {
    lg::error("Provided game data file appears to be too small or corrupted! Size is: {}",
              fs::file_size(path_to_supposed_iso));
    return {false, ExtractorErrorCode::EXTRACTION_ISO_UNEXPECTED_SIZE};
  }
  return {true, ExtractorErrorCode::SUCCESS};
}

std::tuple<uint64_t, int> calculate_extraction_hash(const IsoFile& iso_file) {
  // - XOR all hashes together and hash the result.  This makes the ordering of the hashes (aka
  // files) irrelevant
  uint64_t combined_hash = 0;
  for (const auto& hash : iso_file.hashes) {
    combined_hash ^= hash;
  }
  return {XXH64(&combined_hash, sizeof(uint64_t), 0), iso_file.hashes.size()};
}

std::tuple<uint64_t, int> calculate_extraction_hash(const fs::path& extracted_iso_path) {
  // - XOR all hashes together and hash the result.  This makes the ordering of the hashes (aka
  // files) irrelevant
  uint64_t combined_hash = 0;
  int filec = 0;
  for (auto const& dir_entry : fs::recursive_directory_iterator(extracted_iso_path)) {
    if (dir_entry.is_regular_file()) {
      auto buffer = file_util::read_binary_file(dir_entry.path().string());
      auto hash = XXH64(buffer.data(), buffer.size(), 0);
      combined_hash ^= hash;
      filec++;
    }
  }
  return {XXH64(&combined_hash, sizeof(uint64_t), 0), filec};
}

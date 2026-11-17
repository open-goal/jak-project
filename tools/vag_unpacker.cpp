#include <cstdio>
#include <set>

#include "common/audio/audio_formats.h"
#include "common/audio/vag.h"
#include "common/util/FileUtil.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

#include "fmt/format.h"
#include "third-party/json.hpp"

namespace {

bool seek64(FILE* fp, u64 offset) {
#ifdef _WIN32
  return _fseeki64(fp, (long long)offset, SEEK_SET) == 0;
#else
  return fseeko(fp, (off_t)offset, SEEK_SET) == 0;
#endif
}

u64 file_size64(FILE* fp) {
#ifdef _WIN32
  _fseeki64(fp, 0, SEEK_END);
  return _ftelli64(fp);
#else
  fseeko(fp, 0, SEEK_END);
  return ftello(fp);
#endif
}

struct Wad {
  std::string file_name;  // VAGWAD.ENG
  std::string language;   // ENG
  bool international = false;
};

void print_usage() {
  printf(
      "usage: vag_unpacker <game> <output folder> <VAGDIR file> <VAGWAD files...> [--wav]\n"
      "  game is one of jak1, jak2, jak3, jakx\n"
      "  --wav also writes a .wav next to every .vag, for listening (the .vag is what gets "
      "repacked)\n");
}

}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  printf("OpenGOAL version %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  printf("VAG Unpacking Tool\n");

  std::vector<std::string> args;
  bool write_wavs = false;
  for (int i = 1; i < argc; i++) {
    if (std::string(argv[i]) == "--wav") {
      write_wavs = true;
    } else {
      args.push_back(argv[i]);
    }
  }

  if (args.size() < 4) {
    print_usage();
    return 1;
  }

  auto game = vag::game_from_name(args[0]);
  if (!game) {
    printf("Unknown game '%s'\n", args[0].c_str());
    print_usage();
    return 1;
  }
  const fs::path out_path = args[1];
  const fs::path dir_path = args[2];

  std::vector<u8> dir_data;
  vag::Dir dir;
  try {
    dir_data = file_util::read_binary_file(dir_path);
    dir = vag::parse_dir(*game, dir_data);
  } catch (std::exception& e) {
    printf("Failed to read %s: %s\n", dir_path.string().c_str(), e.what());
    return 1;
  }
  printf("%s has %d entries\n", dir_path.filename().string().c_str(), (int)dir.entries.size());

  // names must be unique because they become file names.
  {
    std::set<std::string> names;
    for (auto& e : dir.entries) {
      if (!names.insert(vag::trim_name(e.name)).second) {
        printf("The directory has two entries named '%s', this is not supported.\n",
               vag::trim_name(e.name).c_str());
        return 1;
      }
    }
  }

  std::vector<Wad> wads;
  std::vector<fs::path> wad_paths;
  for (size_t i = 3; i < args.size(); i++) {
    fs::path p = args[i];
    Wad wad;
    wad.file_name = p.filename().string();
    wad.language = p.extension().string();
    if (!wad.language.empty()) {
      wad.language = wad.language.substr(1);
    }
    // like the decompiler: the INT wad has the international entries, all others the rest.
    wad.international = *game == vag::Game::Jak3 && wad.language == "INT";
    wads.push_back(wad);
    wad_paths.push_back(p);
  }

  const u64 page_size = vag::page_size(*game);

  nlohmann::json description;
  description["game"] = vag::game_name(*game);
  description["dir_file_name"] = dir_path.filename().string();
  description["dir_file_size"] = dir_data.size();
  description["dir_version"] = dir.version;
  description["wads"] = nlohmann::json::array();
  for (auto& wad : wads) {
    description["wads"].push_back(wad.file_name);
  }
  description["wad_sizes"] = nlohmann::json::object();
  description["entries"] = nlohmann::json::array();
  for (auto& e : dir.entries) {
    nlohmann::json entry;
    entry["name"] = vag::trim_name(e.name);
    entry["stereo"] = e.stereo;
    if (*game == vag::Game::Jak3) {
      entry["international"] = e.international;
      entry["param"] = e.param;
    }
    entry["page"] = e.page;
    entry["files"] = nlohmann::json::object();
    description["entries"].push_back(entry);
  }

  int problems = 0;
  for (size_t wad_idx = 0; wad_idx < wads.size(); wad_idx++) {
    const auto& wad = wads[wad_idx];
    FILE* fp = file_util::open_file(wad_paths[wad_idx], "rb");
    if (!fp) {
      printf("Failed to open %s\n", wad_paths[wad_idx].string().c_str());
      return 1;
    }
    const u64 wad_size = file_size64(fp);
    description["wad_sizes"][wad.file_name] = wad_size;
    printf("Unpacking %s\n", wad.file_name.c_str());
    file_util::create_dir_if_needed(out_path / wad.language);

    for (size_t i = 0; i < dir.entries.size(); i++) {
      const auto& e = dir.entries[i];
      if (e.international != wad.international) {
        continue;
      }
      const std::string name = vag::trim_name(e.name);
      const u64 start = e.page * page_size;

      u8 header_bytes[vag::HEADER_SIZE];
      std::optional<vag::Header> header;
      if (start + vag::HEADER_SIZE <= wad_size && seek64(fp, start) &&
          fread(header_bytes, 1, vag::HEADER_SIZE, fp) == vag::HEADER_SIZE) {
        header = vag::parse_header(header_bytes, vag::HEADER_SIZE);
      }
      if (!header) {
        printf("  %s: no VAG at offset 0x%llx, skipping\n", name.c_str(),
               (unsigned long long)start);
        problems++;
        continue;
      }

      const size_t length = vag::entry_length(*header, e.stereo);
      if (start + length > wad_size) {
        printf("  %s: runs past the end of the file, skipping\n", name.c_str());
        problems++;
        continue;
      }

      std::vector<u8> data(length);
      if (!seek64(fp, start) || fread(data.data(), 1, length, fp) != length) {
        printf("  %s: read failed\n", name.c_str());
        problems++;
        continue;
      }

      const std::string relative = fmt::format("{}/{}.vag", wad.language, name);
      file_util::write_binary_file(out_path / relative, data.data(), data.size());
      description["entries"][i]["files"][wad.file_name] = relative;

      if (write_wavs) {
        auto decoded = vag::decode_vag(data.data(), data.size(), e.stereo);
        if (decoded) {
          write_wave_file(decoded->left, decoded->right, decoded->sample_rate,
                          out_path / fmt::format("{}/{}.wav", wad.language, name));
        } else {
          printf("  %s: failed to decode\n", name.c_str());
          problems++;
        }
      }
    }
    fclose(fp);
  }

  file_util::write_text_file(out_path / "vag_description.json", description.dump(2));
  printf("Done, wrote %s\n", (out_path / "vag_description.json").string().c_str());
  if (problems) {
    printf("%d problems, see above\n", problems);
  }
  return 0;
}

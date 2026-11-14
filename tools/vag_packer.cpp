#include <algorithm>
#include <cstdio>
#include <map>

#include "common/audio/audio_formats.h"
#include "common/audio/vag.h"
#include "common/util/FileUtil.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

#include "fmt/format.h"
#include "third-party/json.hpp"

namespace {

void print_usage() {
  printf(
      "usage: vag_packer <path to folder with audio files> <VAG description file>\n"
      "  the description file is the vag_description.json made by vag_unpacker.\n"
      "  every file in it can be a .vag (used as is) or a .wav (16-bit PCM, encoded by this "
      "tool)\n");
}

/*!
 * The data of one entry in one WAD, either already in memory (encoded from a wav) or in a file.
 */
struct Blob {
  fs::path path;
  std::vector<u8> data;
  bool in_memory = false;
  size_t size = 0;

  std::vector<u8> get() const { return in_memory ? data : file_util::read_binary_file(path); }
};

struct PackEntry {
  vag::DirEntry dir_entry;
  bool has_original_page = false;
  u32 original_page = 0;
  std::map<std::string, Blob> blobs;  // by wad file name
  u32 new_page = 0;
};

struct WadInfo {
  std::string file_name;
  bool international = false;
};

std::string lower_extension(const fs::path& p) {
  std::string ext = p.extension().string();
  std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c) { return tolower(c); });
  return ext;
}

bool load_blob(vag::Game game,
               const fs::path& base,
               const std::string& relative,
               const vag::DirEntry& entry,
               Blob* blob,
               std::string* error) {
  blob->path = base / relative;
  if (!fs::exists(blob->path)) {
    *error = "file does not exist";
    return false;
  }

  if (lower_extension(blob->path) == ".wav") {
    WaveData wave;
    if (!read_wave_file(blob->path, &wave, error)) {
      return false;
    }
    if (wave.left_samples.empty()) {
      *error = "the wave file has no samples";
      return false;
    }
    // Convert between mono and stereo if needed. The game plays back stereo files only for
    // entries that are marked as stereo in the directory.
    if (entry.stereo && wave.right_samples.empty()) {
      wave.right_samples = wave.left_samples;
    } else if (!entry.stereo && !wave.right_samples.empty()) {
      for (size_t i = 0; i < wave.left_samples.size(); i++) {
        wave.left_samples[i] = ((s32)wave.left_samples[i] + wave.right_samples[i]) / 2;
      }
      wave.right_samples.clear();
    }
    blob->data = vag::build_vag(game, entry.name, wave.left_samples, wave.right_samples,
                                (u32)wave.sample_rate);
    blob->in_memory = true;
    blob->size = blob->data.size();
    return true;
  }

  // raw file, just check that it looks right.
  auto bytes = file_util::read_binary_file(blob->path);
  auto header = vag::parse_header(bytes.data(), bytes.size());
  if (!header) {
    *error = "not a VAG file (or a wav file without the .wav extension)";
    return false;
  }
  if (bytes.size() < vag::entry_length(*header, entry.stereo)) {
    *error = "the file is shorter than its header says, is stereo set correctly?";
    return false;
  }
  blob->size = bytes.size();
  return true;
}

}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  printf("OpenGOAL version %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  printf("VAG Packing Tool\n");

  if (argc != 3) {
    print_usage();
    return 1;
  }

  const fs::path base_path = argv[1];
  nlohmann::json description;
  try {
    description = nlohmann::json::parse(file_util::read_text_file(fs::path(argv[2])));
  } catch (std::exception& e) {
    printf("Failed to read the description file: %s\n", e.what());
    return 1;
  }

  std::vector<WadInfo> wads;
  std::vector<PackEntry> entries;
  vag::Game game;
  vag::Dir dir;
  std::string dir_file_name;
  size_t dir_file_size = 0;

  try {
    auto g = vag::game_from_name(description.at("game").get<std::string>());
    if (!g) {
      printf("Unknown game in description file\n");
      return 1;
    }
    game = *g;
    dir_file_name = description.at("dir_file_name").get<std::string>();
    dir_file_size = description.value("dir_file_size", 0);
    dir.version = description.value("dir_version", 0);

    for (auto& w : description.at("wads")) {
      WadInfo info;
      info.file_name = w.get<std::string>();
      info.international =
          game == vag::Game::Jak3 && fs::path(info.file_name).extension() == ".INT";
      wads.push_back(info);
    }

    for (auto& j : description.at("entries")) {
      PackEntry e;
      e.dir_entry.name = vag::pad_name(j.at("name").get<std::string>());
      e.dir_entry.stereo = j.value("stereo", false);
      e.dir_entry.international = game == vag::Game::Jak3 && j.value("international", false);
      e.dir_entry.param = j.value("param", 0);
      if (j.contains("page")) {
        e.has_original_page = true;
        e.original_page = j["page"].get<u32>();
      }
      if (auto err = vag::check_name(game, vag::trim_name(e.dir_entry.name))) {
        printf("Bad entry: %s\n", err->c_str());
        return 1;
      }
      for (auto& [wad_name, file] : j.at("files").items()) {
        Blob blob;
        std::string error;
        if (!load_blob(game, base_path, file.get<std::string>(), e.dir_entry, &blob, &error)) {
          printf("%s (%s): %s\n", vag::trim_name(e.dir_entry.name).c_str(),
                 file.get<std::string>().c_str(), error.c_str());
          return 1;
        }
        e.blobs[wad_name] = std::move(blob);
      }
      entries.push_back(std::move(e));
    }
  } catch (std::exception& ex) {
    printf("The description file is invalid: %s\n", ex.what());
    return 1;
  }

  if (entries.size() > vag::max_dir_entries(game)) {
    printf("Too many entries: %d, the game can only load %d\n", (int)entries.size(),
           (int)vag::max_dir_entries(game));
    return 1;
  }

  const u64 page_size = vag::page_size(game);
  auto pages_for = [&](size_t bytes) { return (u32)((bytes + page_size - 1) / page_size); };

  // The directory is shared by all WADs, so an entry must be at the same place in every WAD. Wads
  // for the international entries (Jak 3) are separate from the others and have their own layout.
  for (bool international : {false, true}) {
    std::vector<const WadInfo*> group_wads;
    for (auto& w : wads) {
      if (w.international == international) {
        group_wads.push_back(&w);
      }
    }
    std::vector<PackEntry*> group;
    for (auto& e : entries) {
      if (e.dir_entry.international == international) {
        group.push_back(&e);
      }
    }
    if (group_wads.empty()) {
      // these WADs are not being repacked, keep the entries where they are.
      for (auto* e : group) {
        e->new_page = e->original_page;
      }
      continue;
    }

    for (auto* e : group) {
      for (auto* w : group_wads) {
        if (!e->blobs.count(w->file_name)) {
          printf("%s has no file for %s\n", vag::trim_name(e->dir_entry.name).c_str(),
                 w->file_name.c_str());
          return 1;
        }
      }
    }

    // Keep the original order of the streams. Entries that share an original page share a slot.
    // New entries (no page in the description) each get their own slot at the end.
    std::vector<PackEntry*> order = group;
    std::stable_sort(order.begin(), order.end(), [](PackEntry* a, PackEntry* b) {
      u64 pa = a->has_original_page ? a->original_page : (1ull << 40);
      u64 pb = b->has_original_page ? b->original_page : (1ull << 40);
      return pa < pb;
    });

    // Slots keep at least the size they originally had (this preserves any gaps, so unmodified
    // audio ends up exactly where it was), and grow when the new audio is larger.
    struct Slot {
      size_t first, last;  // range in order
      u32 needed_pages = 0;
      u32 original_pages = 0;
    };
    std::vector<Slot> slots;
    for (size_t i = 0; i < order.size();) {
      Slot slot;
      slot.first = i;
      size_t j = i;
      do {
        for (auto& [wad_name, blob] : order[j]->blobs) {
          slot.needed_pages = std::max(slot.needed_pages, pages_for(blob.size));
        }
        j++;
      } while (j < order.size() && order[i]->has_original_page && order[j]->has_original_page &&
               order[j]->original_page == order[i]->original_page);
      slot.last = j;
      slots.push_back(slot);
      i = j;
    }
    for (size_t i = 0; i + 1 < slots.size(); i++) {
      auto* a = order[slots[i].first];
      auto* b = order[slots[i + 1].first];
      if (a->has_original_page && b->has_original_page) {
        slots[i].original_pages = b->original_page - a->original_page;
      }
    }

    u32 next_page = 0;
    if (!slots.empty() && order[slots[0].first]->has_original_page) {
      next_page = order[slots[0].first]->original_page;
    }
    for (auto& slot : slots) {
      for (size_t k = slot.first; k < slot.last; k++) {
        order[k]->new_page = next_page;
      }
      next_page += std::max(slot.needed_pages, slot.original_pages);
    }

    if (game == vag::Game::Jak3 && next_page > 0x10000) {
      printf("The audio is too large, the directory can only address 0x10000 pages\n");
      return 1;
    }
  }

  // write the directory
  dir.entries.clear();
  for (auto& e : entries) {
    auto de = e.dir_entry;
    de.page = e.new_page;
    dir.entries.push_back(de);
  }
  auto dir_bytes = vag::write_dir(game, dir, dir_file_size);
  const fs::path dir_out = base_path / ("mod_" + dir_file_name);
  file_util::write_binary_file(dir_out, dir_bytes.data(), dir_bytes.size());
  printf("Wrote %s\n", dir_out.string().c_str());

  // write the WADs
  for (auto& wad : wads) {
    std::vector<PackEntry*> in_wad;
    for (auto& e : entries) {
      if (e.dir_entry.international == wad.international) {
        in_wad.push_back(&e);
      }
    }
    std::stable_sort(in_wad.begin(), in_wad.end(),
                     [](PackEntry* a, PackEntry* b) { return a->new_page < b->new_page; });

    const fs::path wad_out = base_path / ("mod_" + wad.file_name);
    FILE* fp = file_util::open_file(wad_out, "wb");
    if (!fp) {
      printf("Failed to open %s for writing\n", wad_out.string().c_str());
      return 1;
    }

    u64 written = 0;
    std::vector<u8> zeros(page_size, 0);
    auto pad_to = [&](u64 target) {
      while (written < target) {
        size_t n = std::min<u64>(zeros.size(), target - written);
        fwrite(zeros.data(), 1, n, fp);
        written += n;
      }
    };

    u32 last_page = UINT32_MAX;
    for (auto* e : in_wad) {
      if (e->new_page == last_page) {
        continue;  // alias of the previous entry
      }
      last_page = e->new_page;
      pad_to((u64)e->new_page * page_size);
      auto data = e->blobs.at(wad.file_name).get();
      fwrite(data.data(), 1, data.size(), fp);
      written += data.size();
    }
    // end on a page boundary, and keep the original file size (with its padding) if we can
    pad_to(((written + page_size - 1) / page_size) * page_size);
    if (description.contains("wad_sizes") && description["wad_sizes"].contains(wad.file_name)) {
      pad_to(description["wad_sizes"][wad.file_name].get<u64>());
    }
    fclose(fp);
    printf("Wrote %s (%llu bytes)\n", wad_out.string().c_str(), (unsigned long long)written);
  }

  printf("Done\n");
  return 0;
}

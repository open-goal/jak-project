#include "read_iso_file.h"

#include "common/common_types.h"
#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "third-party/zstd/lib/common/xxhash.h"

IsoFile::IsoFile() {
  root.is_dir = true;
}

std::string IsoFile::print() const {
  std::string result;
  root.print(&result, "");
  return result;
}

void IsoFile::Entry::print(std::string* result, const std::string& prefix) const {
  if (is_dir) {
    std::string child_prefix = prefix + "/" + name;
    for (const auto& child : children) {
      child.print(result, child_prefix);
    }
  } else {
    result->append(prefix);
    result->push_back('/');
    result->append(name);
    result->push_back('\n');
  }
}

namespace {
constexpr int SECTOR_SIZE = 0x800;

template <typename T>
T read_file(FILE* fp, u32 sector, u32 offset_in_sector) {
  T result;
  if (fseek(fp, sector * SECTOR_SIZE + offset_in_sector, SEEK_SET)) {
    ASSERT_MSG(false, "Failed to fseek iso");
  }
  if (fread(&result, sizeof(T), 1, fp) != 1) {
    ASSERT_MSG(false, "Failed to fread iso");
  }
  return result;
}

void add_from_dir(FILE* fp, u32 sector, u32 size, IsoFile::Entry* parent) {
  u32 offset = 0;
  while (offset < size) {
    if (!read_file<u8>(fp, sector, offset)) {
      offset = (offset & ~(SECTOR_SIZE - 1)) + SECTOR_SIZE;
      continue;
    }
    u8 record_size = read_file<u8>(fp, sector, offset);
    u8 kind = read_file<u8>(fp, sector, offset + 0x21);
    if ((kind != 0) && (kind != 1)) {
      auto& entry = parent->children.emplace_back();
      u32 extent = read_file<u32>(fp, sector, offset + 2);
      u32 dir_or_file_size = read_file<u32>(fp, sector, offset + 10);
      u32 name_len = read_file<u8>(fp, sector, offset + 32);
      u8 c0 = read_file<u8>(fp, sector, offset + name_len + 0x1f);
      u8 c1 = read_file<u8>(fp, sector, offset + name_len + 0x20);
      for (u32 i = 0; i < name_len; i++) {
        entry.name.push_back(read_file<char>(fp, sector, offset + 0x21 + i));
      }
      entry.is_dir = (c0 != ';' || c1 != '1');
      if (entry.is_dir) {
        add_from_dir(fp, extent, dir_or_file_size, &entry);
      } else {
        entry.name.pop_back();
        entry.name.pop_back();
        entry.offset_in_file = SECTOR_SIZE * extent;
        entry.size = dir_or_file_size;
      }
    }
    offset += record_size;
  }
}

void unpack_entry(FILE* fp,
                  IsoFile& iso,
                  const IsoFile::Entry& entry,
                  const fs::path& dest,
                  bool print_progress) {
  fs::path path_to_entry = dest / entry.name;
  if (entry.is_dir) {
    fs::create_directory(path_to_entry);
    for (const auto& child : entry.children) {
      unpack_entry(fp, iso, child, path_to_entry, print_progress);
    }
  } else {
    if (print_progress) {
      lg::info("Extracting {}...", entry.name);
    }
    std::vector<u8> buffer(entry.size);
    if (fseek(fp, entry.offset_in_file, SEEK_SET)) {
      ASSERT_MSG(false, "Failed to fseek iso when unpacking");
    }
    if (fread(buffer.data(), buffer.size(), 1, fp) != 1) {
      ASSERT_MSG(false, "Failed to fread iso when unpacking");
    }
    file_util::write_binary_file(path_to_entry.string(), buffer.data(), buffer.size());
    iso.files_extracted++;
    if (iso.shouldHash) {
      auto hash = XXH64(buffer.data(), buffer.size(), 0);
      iso.hashes.push_back(hash);
    }
  }
}
}  // namespace

IsoFile find_files_in_iso(FILE* fp) {
  IsoFile result;
  u32 path_table_sector = read_file<u32>(fp, 0x10, 0x8c);
  u32 path_table_extent = read_file<u32>(fp, path_table_sector, 2);
  u32 dir_size = read_file<u32>(fp, path_table_extent, 10);
  add_from_dir(fp, path_table_extent, dir_size, &result.root);
  return result;
}

void unpack_iso_files(FILE* fp, IsoFile& layout, const fs::path& dest, bool print_progress) {
  unpack_entry(fp, layout, layout.root, dest, print_progress);
}

IsoFile unpack_iso_files(FILE* fp,
                         const fs::path& dest,
                         bool print_progress,
                         const bool hashFiles) {
  auto file = find_files_in_iso(fp);
  file.shouldHash = hashFiles;
  unpack_iso_files(fp, file, dest, print_progress);
  return file;
}

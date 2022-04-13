#pragma once

#include <string>
#include <vector>
#include <filesystem>

struct IsoFile {
  struct Entry {
    bool is_dir = false;
    std::string name;

    // if file
    size_t offset_in_file = 0;
    size_t size = 0;

    // if dir
    std::vector<Entry> children;
    void print(std::string* result, const std::string& prefix) const;
  };

  std::string print() const;

  Entry root;

  IsoFile();
};

IsoFile find_files_in_iso(FILE* fp);
void unpack_iso_files(FILE* fp, const IsoFile& layout, const std::filesystem::path& dest);
void unpack_iso_files(FILE* fp, const std::filesystem::path& dest);
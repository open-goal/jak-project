#include "file_io.h"
#include <fstream>
#include <sstream>
#include <cassert>

namespace util {
std::string read_text_file(const std::string& path) {
  std::ifstream file(path);
  if (!file.good()) {
    throw std::runtime_error("couldn't open " + path);
  }
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

std::string combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
}

std::string combine_path(std::vector<std::string> path) {
  if (path.empty()) {
    return {};
  }
  std::string result = path.front();
  for (size_t i = 1; i < path.size(); i++) {
    result = combine_path(result, path.at(i));
  }
  return result;
}

void write_binary_file(const std::string& name, void* data, size_t size) {
  FILE* fp = fopen(name.c_str(), "wb");
  if (!fp) {
    throw std::runtime_error("couldn't open file " + name);
  }

  if (fwrite(data, size, 1, fp) != 1) {
    throw std::runtime_error("couldn't write file " + name);
  }

  fclose(fp);
}

}  // namespace util

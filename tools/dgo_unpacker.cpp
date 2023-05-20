#include <cstdio>
#include <stdexcept>

#include "common/util/DgoReader.h"
#include "common/util/FileUtil.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

namespace {
int run(int argc, char** argv) {
  printf("OpenGOAL version %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  printf("DGO Unpacking Tool\n");

  if (argc < 3) {
    printf("usage: dgo_unpacker <output path> <dgo files>\n");
    return 1;
  }

  std::string out_path = argv[1];

  for (int i = 2; i < argc; i++) {
    std::string file_name = argv[i];
    std::string base = file_util::base_name(file_name);
    printf("Unpacking %s\n", base.c_str());
    // read the file
    auto data = file_util::read_binary_file(file_name);
    if (file_util::dgo_header_is_compressed(data)) {
      printf(" Detected compressed dgo, decompressing...\n");
      auto original_size = data.size();
      data = file_util::decompress_dgo(data);
      printf(" Decompressed from %d to %d bytes (%.2f%% compression)\n", int(original_size),
             int(data.size()), 100.f * original_size / data.size());
    }
    // read as a DGO
    auto dgo = DgoReader(base, data);
    // write dgo description
    file_util::create_dir_if_needed(out_path);
    file_util::write_text_file(file_util::combine_path(out_path, base + ".txt"),
                               dgo.description_as_json());
    // write files:
    for (auto& entry : dgo.entries()) {
      file_util::write_binary_file(file_util::combine_path(out_path, entry.unique_name),
                                   (const void*)entry.data.data(), entry.data.size());
    }
  }

  printf("Done\n");
  return 0;
}
}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  try {
    return run(argc, argv);
  } catch (const std::exception& e) {
    printf("An error occurred: %s\n", e.what());
    return 1;
  }
}

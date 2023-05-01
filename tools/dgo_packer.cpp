#include <cstdio>

#include "common/util/BinaryWriter.h"
#include "common/util/FileUtil.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

#include "third-party/json.hpp"

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  printf("OpenGOAL version %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  printf("DGO Packing Tool\n");

  if (argc < 3) {
    printf("usage: dgo_packer <path> <dgo description file>\n");
    return 1;
  }

  std::string out_path = argv[1];

  for (int i = 2; i < argc; i++) {
    std::string file_name = argv[i];
    std::string file_text = file_util::read_text_file(file_name);

    auto x = nlohmann::json::parse(file_text);
    std::string out_file_name = x["file_name"];
    std::string internal_name = x["internal_name"];
    printf("Packing %s\n", internal_name.c_str());

    BinaryWriter writer;
    writer.add<u32>(x["objects"].size());
    writer.add_cstr_len(x["internal_name"].get<std::string>().c_str(), 60);

    for (auto& entry : x["objects"]) {
      auto obj_data =
          file_util::read_binary_file(file_util::combine_path(out_path, entry["unique_name"]));
      auto aligned_size = ((obj_data.size() + 15) / 16) * 16;
      // size
      writer.add<uint32_t>(aligned_size);
      // name
      writer.add_str_len(entry["internal_name"].get<std::string>().c_str(), 60);
      // data
      writer.add_data(obj_data.data(), obj_data.size());
      // pad
      while (writer.get_size() & 15) {
        writer.add<uint8_t>(0);
      }
    }
    writer.write_to_file(file_util::combine_path(out_path, "mod_" + out_file_name));
  }

  printf("Done\n");
  return 0;
}

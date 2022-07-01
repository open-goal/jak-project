/*!
 * @file DgoWriter.cpp
 * Create a DGO from existing files.
 */

#include "DgoWriter.h"

#include "BinaryWriter.h"
#include "FileUtil.h"

void build_dgo(const DgoDescription& description, const std::string& output_prefix) {
  BinaryWriter writer;
  // dgo header
  writer.add<uint32_t>(description.entries.size());
  writer.add_cstr_len(description.dgo_name.c_str(), 60);

  for (auto& obj : description.entries) {
    auto obj_data = file_util::read_binary_file(file_util::get_jak_project_dir() / "out" /
                                                output_prefix / "obj" / obj.file_name);
    // size
    writer.add<uint32_t>(obj_data.size());
    // name
    writer.add_str_len(obj.name_in_dgo, 60);
    // data
    writer.add_data(obj_data.data(), obj_data.size());
    // pad
    while (writer.get_size() & 0xf) {
      writer.add<uint8_t>(0);
    }
  }

  writer.write_to_file(file_util::get_jak_project_dir() / "out" / output_prefix / "iso" /
                       description.dgo_name);
}

#include "dir_tpages.h"

#include "DataObjectGenerator.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"

void compile_dir_tpages(const std::string& filename) {
  std::vector<int> lengths;

  goos::Reader reader;
  auto code = reader.read_from_file({filename});
  std::string err;

  goos::for_each_in_list(code.as_pair()->cdr, [&](const goos::Object& obj) {
    if (!obj.is_int()) {
      throw std::runtime_error("Invalid tpage dir file");
    }
    lengths.push_back(obj.as_int());
  });

  DataObjectGenerator gen;
  gen.add_type_tag("texture-page-dir");
  gen.add_word(lengths.size());
  for (auto len : lengths) {
    gen.add_word(len);
    gen.add_symbol_link("#f");
    gen.add_symbol_link("#f");
  }

  auto data = gen.generate_v4();
  file_util::create_dir_if_needed(file_util::get_file_path({"out", "obj"}));
  file_util::write_binary_file(file_util::get_file_path({"out", "obj", "dir-tpages.go"}),
                               data.data(), data.size());
}

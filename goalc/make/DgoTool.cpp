#include <filesystem>
#include <stdexcept>

#include "DgoTool.h"
#include "common/goos/ParseHelpers.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"
#include "third-party/fmt/core.h"

namespace {
DgoDescription parse_desc_file(const std::string& filename, goos::Reader& reader) {
  auto dgo_desc = reader.read_from_file({filename}).as_pair()->cdr;
  if (goos::list_length(dgo_desc) != 1) {
    throw std::runtime_error("Invalid DGO description - got too many lists");
  }
  auto dgo = dgo_desc.as_pair()->car;

  DgoDescription desc;
  auto first = dgo.as_pair()->car;
  desc.dgo_name = first.as_string()->data;
  auto dgo_rest = dgo.as_pair()->cdr;

  for_each_in_list(dgo_rest, [&](const goos::Object& entry) {
    goos::Arguments e_arg;
    std::string err;
    if (!goos::get_va(entry, &err, &e_arg)) {
      throw std::runtime_error(fmt::format("Invalid DGO description: {}\n", err));
    }

    if (!goos::va_check(e_arg, {goos::ObjectType::STRING, goos::ObjectType::STRING}, {}, &err)) {
      throw std::runtime_error(fmt::format("Invalid DGO description: {}\n", err));
    }
    DgoDescription::DgoEntry o;
    o.file_name = e_arg.unnamed.at(0).as_string()->data;
    o.name_in_dgo = e_arg.unnamed.at(1).as_string()->data;
    desc.entries.push_back(o);
  });
  return desc;
}
}  // namespace

DgoTool::DgoTool() : Tool("dgo") {}

bool DgoTool::run(const ToolInput& task) {
  auto desc = parse_desc_file(task.input, m_reader);
  build_dgo(desc);
  return true;
}

std::vector<std::string> DgoTool::get_additional_dependencies(const ToolInput& task) {
  std::vector<std::string> result;
  auto desc = parse_desc_file(task.input, m_reader);
  for (auto& x : desc.entries) {
    result.push_back(fmt::format("out/obj/{}", x.file_name));
  }
  return result;
}

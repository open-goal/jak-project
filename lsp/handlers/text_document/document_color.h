#pragma once

#include <optional>

#include "common/util/string_util.h"

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/document_color.h"

float hexToFloat(const std::string& hex) {
  int value = std::stoi(hex, nullptr, 16);
  return static_cast<float>(value) / 255.0f;
}

std::optional<LSPSpec::Color> color_hexstring_to_lsp_color(const std::string& color_name) {
  if (!str_util::contains(color_name, "#")) {
    return {};
  }
  const auto color_tokens = str_util::split(color_name, '#');
  const auto hexstring = color_tokens.at(1);
  std::string red_hex = hexstring.substr(0, 2);
  std::string green_hex = hexstring.substr(2, 2);
  std::string blue_hex = hexstring.substr(4, 2);

  float red = hexToFloat(red_hex);
  float green = hexToFloat(green_hex);
  float blue = hexToFloat(blue_hex);

  return LSPSpec::Color{red, green, blue, 1.0};
}

std::optional<json> document_color_handler(Workspace& workspace, int id, json raw_params) {
  auto params = raw_params.get<LSPSpec::DocumentColorParams>();
  json colors = json::array();

  // TODO - hex strings aren't desirable in the `font-color` enum
  // this could be used for the `new 'static 'rgba` instances but that requires proper
  // AST support as it cannot (and should not) be assumed that all 4 components will be on the same
  // line
  return colors;

  //// Iterate through document, mark text colors ourselves
  // auto file_type = workspace.determine_filetype_from_uri(params.textDocument.m_uri);

  // if (file_type == Workspace::FileType::OpenGOAL) {
  //   auto tracked_file = workspace.get_tracked_og_file(params.textDocument.m_uri);
  //   if (!tracked_file) {
  //     return {};
  //   }

  //  // This is something that is ok to be a regex, because it's very niche
  //  for (int i = 0; i < tracked_file->m_lines.size(); i++) {
  //    const auto& line = tracked_file->m_lines.at(i);
  //    std::smatch matches;
  //    std::regex regex("\\(font-color ([^)]*)\\)");

  //    std::sregex_iterator iter(line.begin(), line.end(), regex);
  //    std::sregex_iterator end;

  //    for (; iter != end; iter++) {
  //      std::smatch match = *iter;
  //      std::string capture_group = match.str(1);
  //      LSPSpec::ColorInformation color_info;
  //      color_info.range = {{i, match.position(1)}, {i, match.position(1) + match.size()}};
  //      const auto color = color_hexstring_to_lsp_color(capture_group);
  //      if (!color) {
  //        continue;
  //      }
  //      color_info.color = color.value();
  //      colors.push_back(color_info);
  //      lg::debug("color - {}", capture_group);
  //    }
  //  }
  //}

  // return colors;
}

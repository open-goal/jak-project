#include "lsp/protocol/document_color.h"

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/state/workspace.h"

int hex_to_dec(const std::string& hex) {
  std::string cleaned_string = hex;
  if (cleaned_string.starts_with("#x")) {
    cleaned_string = cleaned_string.substr(2);
  }
  return std::stoi(cleaned_string, nullptr, 16);
}

std::unordered_map<GameVersion, std::unordered_map<int, std::tuple<float, float, float, float>>>
    game_font_colors = {{GameVersion::Jak1,
                         {
                             {0, {223.0, 239.0, 223.0, 255.0}},  {1, {255.0, 255.0, 255.0, 255.0}},
                             {2, {255.0, 255.0, 255.0, 127.0}},  {3, {255.0, 191.0, 63.0, 255.0}},
                             {4, {255.0, 199.0, 0.0, 255.0}},    {5, {255.0, 255.0, 0.0, 255.0}},
                             {6, {63.0, 255.0, 63.0, 255.0}},    {7, {127.0, 127.0, 255.0, 255.0}},
                             {8, {-1.0, 255.0, 255.0, 255.0}},   {9, {255.0, 127.0, 255.0, 255.0}},
                             {10, {191.0, 255.0, 255.0, 255.0}}, {11, {127.0, 191.0, 191.0, 255.0}},
                             {12, {255.0, 255.0, 255.0, 255.0}}, {13, {159.0, 159.0, 159.0, 255.0}},
                             {14, {255.0, 167.0, 0.0, 255.0}},   {15, {223.0, 255.0, 95.0, 255.0}},
                             {16, {143.0, 175.0, 15.0, 255.0}},  {17, {175.0, 191.0, 175.0, 255.0}},
                             {18, {127.0, 143.0, 127.0, 255.0}}, {19, {95.0, 63.0, 95.0, 255.0}},
                             {20, {255.0, 241.0, 143.0, 255.0}}, {21, {63.0, 187.0, 239.0, 255.0}},
                             {22, {57.0, 57.0, 57.0, 255.0}},    {23, {127.0, 127.0, 127.0, 255.0}},
                             {24, {243.0, 153.0, 201.0, 255.0}}, {25, {243.0, 103.0, 103.0, 255.0}},
                             {26, {31.0, 201.0, 151.0, 255.0}},  {27, {139.0, 147.0, 239.0, 255.0}},
                             {28, {173.0, 251.0, 255.0, 255.0}}, {29, {253.0, 245.0, 101.0, 255.0}},
                             {30, {241.0, 241.0, 3.0, 255.0}},   {31, {141.0, 207.0, 243.0, 255.0}},
                             {32, {223.0, 239.0, 223.0, 255.0}}, {33, {191.0, -1.0, 0.0, 255.0}},
                             {34, {255.0, 191.0, 63.0, 255.0}},
                         }},
                        {GameVersion::Jak2,
                         {
                             {0, {223.0, 239.0, 223.0, 255.0}},  {1, {255.0, 255.0, 255.0, 255.0}},
                             {2, {255.0, 255.0, 255.0, 127.0}},  {3, {255.0, 63.0, 0.0, 255.0}},
                             {4, {255.0, 199.0, 0.0, 255.0}},    {5, {255.0, 255.0, 0.0, 255.0}},
                             {6, {63.0, 255.0, 63.0, 255.0}},    {7, {0.0, 63.0, 255.0, 255.0}},
                             {8, {0.0, 255.0, 255.0, 255.0}},    {9, {255.0, 127.0, 255.0, 255.0}},
                             {10, {191.0, 255.0, 255.0, 255.0}}, {11, {127.0, 191.0, 191.0, 255.0}},
                             {12, {255.0, 255.0, 255.0, 255.0}}, {13, {159.0, 159.0, 159.0, 255.0}},
                             {14, {255.0, 167.0, 0.0, 255.0}},   {15, {223.0, 255.0, 95.0, 255.0}},
                             {16, {143.0, 175.0, 31.0, 255.0}},  {17, {175.0, 191.0, 175.0, 255.0}},
                             {18, {127.0, 143.0, 127.0, 255.0}}, {19, {95.0, 63.0, 95.0, 255.0}},
                             {20, {255.0, 241.0, 143.0, 255.0}}, {21, {63.0, 187.0, 239.0, 255.0}},
                             {22, {57.0, 57.0, 57.0, 255.0}},    {23, {127.0, 127.0, 127.0, 255.0}},
                             {24, {243.0, 153.0, 201.0, 255.0}}, {25, {243.0, 103.0, 103.0, 255.0}},
                             {26, {31.0, 201.0, 151.0, 255.0}},  {27, {139.0, 147.0, 239.0, 255.0}},
                             {28, {173.0, 251.0, 255.0, 255.0}}, {29, {253.0, 245.0, 101.0, 255.0}},
                             {30, {241.0, 241.0, 3.0, 255.0}},   {31, {141.0, 207.0, 243.0, 255.0}},
                             {32, {127.0, 255.0, 255.0, 255.0}}, {33, {127.0, 255.0, 255.0, 255.0}},
                             {34, {255.0, 255.0, 255.0, 255.0}}, {35, {63.0, 127.0, 127.0, 191.0}},
                             {36, {223.0, 239.0, 223.0, 255.0}}, {37, {191.0, 0.0, 0.0, 255.0}},
                             {38, {255.0, 191.0, 63.0, 255.0}},  {39, {0.0, 0.0, 1.0, 255.0}},
                         }}};

namespace lsp_handlers {

std::optional<json> document_color(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::DocumentColorParams>();
  auto file_type = workspace.determine_filetype_from_uri(params.textDocument.m_uri);
  const auto game_version = workspace.determine_game_version_from_uri(params.textDocument.m_uri);

  json colors = json::array();

  if (!game_version || file_type != Workspace::FileType::OpenGOAL) {
    return colors;
  }

  auto maybe_tracked_file = workspace.get_tracked_og_file(params.textDocument.m_uri);
  if (!maybe_tracked_file) {
    return colors;
  }
  const auto& tracked_file = maybe_tracked_file.value().get();

  // Search for `(new 'static 'rgba....` forms as these can be colored
  // for example - `(new 'static 'rgba :r #x70 :g #x78 :b #x70 :a #x80)`
  const auto rgba_results =
      tracked_file.search_for_forms_that_begin_with({"(", "new", "'static", "'rgba"});
  for (const auto& result : rgba_results) {
    // Iterate the forms and find the color and alpha info
    float red = 0.0f;
    float green = 0.0f;
    float blue = 0.0f;
    float alpha = 0.0f;
    unsigned int token_idx = 0;
    while (token_idx < result.tokens.size()) {
      const auto& token = result.tokens[token_idx];
      // in OpenGOAL -- 255 is equal to 128, so we double every value and subtract 1
      if (token == ":r" && result.tokens.size() > token_idx + 1) {
        red = static_cast<float>((hex_to_dec(result.tokens[token_idx + 1]) * 2) - 1) / 255.0f;
      } else if (token == ":g" && result.tokens.size() > token_idx + 1) {
        green = static_cast<float>((hex_to_dec(result.tokens[token_idx + 1]) * 2) - 1) / 255.0f;
      } else if (token == ":b" && result.tokens.size() > token_idx + 1) {
        blue = static_cast<float>((hex_to_dec(result.tokens[token_idx + 1]) * 2) - 1) / 255.0f;
      } else if (token == ":a" && result.tokens.size() > token_idx + 1) {
        alpha = static_cast<float>((hex_to_dec(result.tokens[token_idx + 1]) * 2) - 1) / 255.0f;
      }
      token_idx++;
    }
    LSPSpec::ColorInformation color_info;
    color_info.range = {{(uint32_t)result.start_point.first, (uint32_t)result.start_point.second},
                        {(uint32_t)result.end_point.first, (uint32_t)result.end_point.second}};
    color_info.color = LSPSpec::Color{red, green, blue, alpha};
    colors.push_back(color_info);
  }
  // Also search for the `(static-rgba ...` macro
  const auto static_rgba_results =
      tracked_file.search_for_forms_that_begin_with({"(", "static-rgba"});
  for (const auto& result : static_rgba_results) {
    float red = static_cast<float>((hex_to_dec(result.tokens[2]) * 2) - 1) / 255.0f;
    float green = static_cast<float>((hex_to_dec(result.tokens[3]) * 2) - 1) / 255.0f;
    float blue = static_cast<float>((hex_to_dec(result.tokens[4]) * 2) - 1) / 255.0f;
    float alpha = static_cast<float>((hex_to_dec(result.tokens[5]) * 2) - 1) / 255.0f;
    LSPSpec::ColorInformation color_info;
    color_info.range = {{(uint32_t)result.start_point.first, (uint32_t)result.start_point.second},
                        {(uint32_t)result.end_point.first, (uint32_t)result.end_point.second}};
    color_info.color = LSPSpec::Color{red, green, blue, alpha};
    colors.push_back(color_info);
  }

  // Search for `(font-color ...` forms
  const auto font_color_results =
      tracked_file.search_for_forms_that_begin_with({"(", "font-color"});
  const auto font_color_enum_entries =
      workspace.get_enum_entries("font-color", game_version.value());
  if (!font_color_enum_entries.empty() &&
      game_font_colors.find(game_version.value()) != game_font_colors.end()) {
    for (const auto& result : font_color_results) {
      const auto font_color = result.tokens[2];
      if (font_color_enum_entries.find(font_color) != font_color_enum_entries.end()) {
        const auto font_color_val = font_color_enum_entries.at(font_color);
        if (game_font_colors[game_version.value()].find(font_color_val) !=
            game_font_colors[game_version.value()].end()) {
          const auto& [red, green, blue, alpha] =
              game_font_colors[game_version.value()].at(font_color_val);
          LSPSpec::ColorInformation color_info;
          color_info.range = {
              {(uint32_t)result.start_point.first, (uint32_t)result.start_point.second},
              {(uint32_t)result.end_point.first, (uint32_t)result.end_point.second}};
          color_info.color =
              LSPSpec::Color{red / 255.0f, green / 255.0f, blue / 255.0f, alpha / 255.0f};
          colors.push_back(color_info);
        }
      }
    }
  }

  return colors;
}

}  // namespace lsp_handlers

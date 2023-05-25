#include "hover.h"

void LSPSpec::to_json(json& j, const Hover& obj) {
  j = json{{"contents", obj.m_contents}};
  if (obj.m_range) {
    j["range"] = obj.m_range.value();
  }
}

void LSPSpec::from_json(const json& j, Hover& obj) {
  j.at("contents").get_to(obj.m_contents);
  if (j.contains("range")) {
    obj.m_range = std::make_optional(j.at("range").get<Range>());
  }
}

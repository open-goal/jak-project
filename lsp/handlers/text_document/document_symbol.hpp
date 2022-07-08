#include <optional>

#include "third-party/json.hpp"

using json = nlohmann::json;

std::optional<json> initialize_handler(int id, json params) {
  InitializeResult result;
  return result.to_json();
}


//json range;
//range["start"] = json{{"line", 2}, {"character", 0}};
//range["end"] = json{{"line", 291}, {"character", 0}};
//
//json selectionRange;
//selectionRange["start"] = json{{"line", 3}, {"character", 12}};
//selectionRange["end"] = json{{"line", 3}, {"character", 27}};
//
//resp[0] = json{{"name", "points-in-air?"},
//               {"kind", 12},
//               {"range", range},
//               {"selectionRange", selectionRange}
//
//};
//json result_body{{"id", body["id"]}, {"result", resp}};

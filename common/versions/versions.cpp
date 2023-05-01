#include "versions.h"

#include "common/util/Assert.h"
#include "common/versions/revision.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/format.h"

GameVersion game_name_to_version(const std::string& name) {
  if (name == "jak1") {
    return GameVersion::Jak1;
  } else if (name == "jak2") {
    return GameVersion::Jak2;
  } else {
    ASSERT_MSG(false, fmt::format("invalid game name: {}", name));
  }
}

bool valid_game_version(const std::string& name) {
  return name == "jak1" || name == "jak2";
}

std::string version_to_game_name(GameVersion v) {
  switch (v) {
    case GameVersion::Jak1:
      return "jak1";
    case GameVersion::Jak2:
      return "jak2";
    default:
      ASSERT_MSG(false, fmt::format("no game_name for version: {} found", fmt::underlying(v)));
  }
}

std::string version_to_game_name_external(GameVersion v) {
  switch (v) {
    case GameVersion::Jak1:
      return "Jak 1";
    case GameVersion::Jak2:
      return "Jak 2";
    default:
      ASSERT_MSG(false, fmt::format("no game_name for version: {} found", fmt::underlying(v)));
  }
}

std::vector<std::string> valid_game_version_names() {
  return {game_version_names[GameVersion::Jak1], game_version_names[GameVersion::Jak2]};
}

std::string build_revision() {
  if (std::string(BUILT_TAG) != "") {
    return std::string(BUILT_TAG);
  }
  if (std::string(BUILT_SHA) != "") {
    return std::string(BUILT_SHA);
  }
  return "Unknown Revision";
}

#include "discord.h"

#include <cstring>
#include <iomanip>
#include <map>
#include <sstream>

#include "common/log/log.h"

#include "discord_jak3.h"
#include "game/runtime.h"

int gDiscordRpcEnabled;
int64_t gStartTime;

static const std::map<GameVersion, std::string> rpc_client_ids = {
    {GameVersion::Jak1, "938876425585434654"},
    {GameVersion::Jak2, "1060390251694149703"},
    {GameVersion::Jak3, "1226307413355790416"}};

void handleDiscordReady(const DiscordUser* user) {
  lg::info("Discord: connected to user {}#{} - {}", user->username, user->discriminator,
           user->userId);
}

void handleDiscordDisconnected(int errcode, const char* message) {
  lg::info("Discord: disconnected ({}: {})", errcode, message);
}

void handleDiscordError(int errcode, const char* message) {
  lg::info("Discord: error ({}: {})", errcode, message);
}

void handleDiscordJoin(const char* /*secret*/) {}
void handleDiscordJoinRequest(const DiscordUser* /*request*/) {}
void handleDiscordSpectate(const char* /*secret*/) {}

void init_discord_rpc() {
  if (g_game_version != GameVersion::Jak1 && g_game_version != GameVersion::Jak2 &&
      g_game_version != GameVersion::Jak3) {
    lg::error("Game version unsupported for Discord RPC - {}", fmt::underlying(g_game_version));
    return;
  }
  if (g_game_version == GameVersion::Jak3) {
    jak3::remap_hack();
  }
  gDiscordRpcEnabled = 1;
  DiscordEventHandlers handlers;
  memset(&handlers, 0, sizeof(handlers));
  handlers.ready = handleDiscordReady;
  handlers.disconnected = handleDiscordDisconnected;
  handlers.errored = handleDiscordError;
  handlers.joinGame = handleDiscordJoin;
  handlers.joinRequest = handleDiscordJoinRequest;
  handlers.spectateGame = handleDiscordSpectate;
  Discord_Initialize(rpc_client_ids.at(g_game_version).c_str(), &handlers, 1, NULL);
}

void set_discord_rpc(int state) {
  gDiscordRpcEnabled = state;
}

// get full level name from symbol name ("village1" -> "Sandover Village")
const char* get_full_level_name(const std::map<std::string, std::string>& level_names,
                                const std::map<std::string, std::string>& level_name_remap,
                                const char* level_name) {
  // ignore sublevels
  auto it = level_name_remap.find(level_name);
  auto actual_level_name = it == level_name_remap.end() ? level_name : it->second;

  const auto& nice_name = level_names.find(actual_level_name);
  if (nice_name != level_names.end()) {
    return nice_name->second.c_str();
  }
  return "unknown";
}

// convert time of day float to a 24-hour hh:mm format string
std::string get_time_of_day(float time) {
  int hour = static_cast<int>(time);
  int minutes = static_cast<int>((time - hour) * 60);
  std::stringstream ss;
  ss << hour << ':' << std::setw(2) << std::setfill('0') << minutes;
  return ss.str();
}

// are we in an area that's indoors, i.e. not affected by time of day?
bool indoors(std::vector<std::string> indoor_levels, const char* level_name) {
  return std::find(indoor_levels.begin(), indoor_levels.end(), level_name) != indoor_levels.end();
}

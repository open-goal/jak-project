#include "discord.h"

#include <cstring>
#include <iomanip>
#include <map>
#include <sstream>
#include <string>

#include "common/log/log.h"

#include "game/runtime.h"

int gDiscordRpcEnabled;
int64_t gStartTime;

static const std::map<GameVersion, std::string> rpc_client_ids = {
    {GameVersion::Jak1, "938876425585434654"},
    {GameVersion::Jak2, "1060390251694149703"}};

static const std::map<std::string, std::string> jak1_level_names = {
    {"intro", "Intro"},
    {"title", "Title screen"},
    {"training", "Geyser Rock"},
    {"village1", "Sandover Village"},
    {"beach", "Sentinel Beach"},
    {"jungle", "Forbidden Jungle"},
    {"misty", "Misty Island"},
    {"firecanyon", "Fire Canyon"},
    {"village2", "Rock Village"},
    {"swamp", "Boggy Swamp"},
    {"rolling", "Precursor Basin"},
    {"sunken", "Lost Precursor City"},
    {"ogre", "Mountain Pass"},
    {"village3", "Volcanic Crater"},
    {"snow", "Snowy Mountain"},
    {"maincave", "Spider Cave"},
    {"lavatube", "Lava Tube"},
    {"citadel", "Gol and Maia's Citadel"},
    {"finalboss", "Final Boss"}};

static const std::map<std::string, std::string> jak1_level_name_remap = {{"jungleb", "jungle"},
                                                                         {"sunkenb", "sunken"},
                                                                         {"robocave", "maincave"},
                                                                         {"darkcave", "maincave"}};

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
  if (g_game_version != GameVersion::Jak1 && g_game_version != GameVersion::Jak2) {
    lg::error("Game version unsupported for Discord RPC - {}", fmt::underlying(g_game_version));
    return;
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
const char* jak1_get_full_level_name(const char* level_name) {
  // ignore sublevels
  auto it = jak1_level_name_remap.find(level_name);
  auto actual_level_name = it == jak1_level_name_remap.end() ? level_name : it->second;

  const auto& nice_name = jak1_level_names.find(actual_level_name);
  if (nice_name != jak1_level_names.end()) {
    return nice_name->second.c_str();
  }
  return "unknown";
};

// time of day string to append to level name for icons
const char* time_of_day_str(float time) {
  int hour = static_cast<int>(time);

  if (hour >= 0 && hour <= 9) {
    return "green-sun";
  } else if (hour < 22) {
    return "day";
  } else if (hour < 25) {
    return "evening";
  } else {
    return "";
  }
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
int indoors(const char* level_name) {
  return !strcmp(level_name, "intro") || !strcmp(level_name, "title") ||
         !strcmp(level_name, "jungleb") || !strcmp(level_name, "sunken") ||
         !strcmp(level_name, "sunkenb") || !strcmp(level_name, "maincave") ||
         !strcmp(level_name, "robocave") || !strcmp(level_name, "darkcave") ||
         !strcmp(level_name, "lavatube") || !strcmp(level_name, "citadel");
}

#include <map>
#include <cstring>
#include <string>

#include "discord.h"

int gDiscordRpcEnabled;
int64_t gStartTime;
static const char* APPLICATION_ID = "938876425585434654";
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

void init_discord_rpc() {
  gDiscordRpcEnabled = 1;
  DiscordEventHandlers handlers;
  memset(&handlers, 0, sizeof(handlers));
  handlers.ready = handleDiscordReady;
  handlers.disconnected = handleDiscordDisconnected;
  handlers.errored = handleDiscordError;
  handlers.joinGame = handleDiscordJoin;
  handlers.joinRequest = handleDiscordJoinRequest;
  handlers.spectateGame = handleDiscordSpectate;
  Discord_Initialize(APPLICATION_ID, &handlers, 1, NULL);
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
  return "Unknown";
};

void handleDiscordReady(const DiscordUser* user) {
  printf("\nDiscord: connected to user %s#%s - %s\n", user->username, user->discriminator,
         user->userId);
}

void handleDiscordDisconnected(int errcode, const char* message) {
  printf("\nDiscord: disconnected (%d: %s)\n", errcode, message);
}

void handleDiscordError(int errcode, const char* message) {
  printf("\nDiscord: error (%d: %s)\n", errcode, message);
}

void handleDiscordJoin(const char* /*secret*/) {}
void handleDiscordJoinRequest(const DiscordUser* /*request*/) {}
void handleDiscordSpectate(const char* /*secret*/) {}

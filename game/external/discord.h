#pragma once

#include <algorithm>
#include <cstring>
#include <map>
#include <string>

#include "common/versions/versions.h"

#include "third-party/discord-rpc/include/discord_rpc.h"

extern int gDiscordRpcEnabled;
extern int64_t gStartTime;

void init_discord_rpc();
void set_discord_rpc(int state);
std::string get_time_of_day(float time);
const char* get_full_level_name(const std::map<std::string, std::string>& level_names,
                                const std::map<std::string, std::string>& level_name_remap,
                                const char* level_name);
bool indoors(std::vector<std::string> indoor_levels, const char* level_name);

void handleDiscordReady(const DiscordUser* user);
void handleDiscordDisconnected(int errcode, const char* message);
void handleDiscordError(int errcode, const char* message);
void handleDiscordJoin(const char* secret);
void handleDiscordJoinRequest(const DiscordUser* request);
void handleDiscordSpectate(const char* secret);

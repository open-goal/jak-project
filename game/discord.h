#pragma once

#include <string>

#include "common/versions.h"

#include "third-party/discord-rpc/include/discord_rpc.h"

extern int gDiscordRpcEnabled;
extern int64_t gStartTime;

void init_discord_rpc();
void set_discord_rpc(int state);
const char* jak1_get_full_level_name(const char* level_name);
const char* time_of_day_str(float time);
std::string get_time_of_day(float time);
int indoors(const char* level_name);

void handleDiscordReady(const DiscordUser* user);
void handleDiscordDisconnected(int errcode, const char* message);
void handleDiscordError(int errcode, const char* message);
void handleDiscordJoin(const char* secret);
void handleDiscordJoinRequest(const DiscordUser* request);
void handleDiscordSpectate(const char* secret);

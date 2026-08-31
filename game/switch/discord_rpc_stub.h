#pragma once

/*!
 * @file discord_rpc_stub.h
 * Drop-in replacement for third-party/discord-rpc/include/discord_rpc.h on Switch. There is no
 * Discord client on a Switch (and no possible backend for one), but the game's kmachine.cpp
 * files across all four titles call directly into this SDK's C API to update rich presence, so
 * rather than touch every call site this mirrors the real header's public surface with no-op
 * bodies -- same shape as the xdbg.cpp debugger stub used elsewhere in this port.
 */
#include <stdint.h>

typedef struct DiscordRichPresence {
  const char* state;
  const char* details;
  int64_t startTimestamp;
  int64_t endTimestamp;
  const char* largeImageKey;
  const char* largeImageText;
  const char* smallImageKey;
  const char* smallImageText;
  const char* partyId;
  int partySize;
  int partyMax;
  int partyPrivacy;
  const char* matchSecret;
  const char* joinSecret;
  const char* spectateSecret;
  int8_t instance;
} DiscordRichPresence;

typedef struct DiscordUser {
  const char* userId;
  const char* username;
  const char* discriminator;
  const char* avatar;
} DiscordUser;

typedef struct DiscordEventHandlers {
  void (*ready)(const DiscordUser* request);
  void (*disconnected)(int errorCode, const char* message);
  void (*errored)(int errorCode, const char* message);
  void (*joinGame)(const char* joinSecret);
  void (*spectateGame)(const char* spectateSecret);
  void (*joinRequest)(const DiscordUser* request);
} DiscordEventHandlers;

#define DISCORD_REPLY_NO 0
#define DISCORD_REPLY_YES 1
#define DISCORD_REPLY_IGNORE 2
#define DISCORD_PARTY_PRIVATE 0
#define DISCORD_PARTY_PUBLIC 1

inline void Discord_Initialize(const char*, DiscordEventHandlers*, int, const char*) {}
inline void Discord_Shutdown(void) {}
inline void Discord_RunCallbacks(void) {}
inline void Discord_UpdateConnection(void) {}
inline void Discord_UpdatePresence(const DiscordRichPresence*) {}
inline void Discord_ClearPresence(void) {}
inline void Discord_Respond(const char*, int) {}
inline void Discord_UpdateHandlers(DiscordEventHandlers*) {}

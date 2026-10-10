#pragma once

#include "common/serialization/subtitles/subtitles.h"
#include "common/serialization/subtitles/subtitles_v2.h"
#include "common/util/json_util.h"

struct SubtitleCutsceneLineMetadataV1 {
  int frame_start;
  bool offscreen;
  std::string speaker;
  bool clear;
};
void to_json(json& j, const SubtitleCutsceneLineMetadataV1& obj);
void from_json(const json& j, SubtitleCutsceneLineMetadataV1& obj);

struct SubtitleHintLineMetadataV1 {
  int frame_start;
  std::string speaker;
  bool clear;
};
void to_json(json& j, const SubtitleHintLineMetadataV1& obj);
void from_json(const json& j, SubtitleHintLineMetadataV1& obj);

struct SubtitleHintMetadataV1 {
  std::string id;  // hex
  std::vector<SubtitleHintLineMetadataV1> lines;
};
void to_json(json& j, const SubtitleHintMetadataV1& obj);
void from_json(const json& j, SubtitleHintMetadataV1& obj);

struct SubtitleMetadataFileV1 {
  std::unordered_map<std::string, std::vector<SubtitleCutsceneLineMetadataV1>> cutscenes;
  std::unordered_map<std::string, SubtitleHintMetadataV1> hints;
};
void to_json(json& j, const SubtitleMetadataFileV1& obj);
void from_json(const json& j, SubtitleMetadataFileV1& obj);

struct SubtitleFileV1 {
  std::unordered_map<std::string, std::string> speakers;
  std::unordered_map<std::string, std::vector<std::string>> cutscenes;
  std::unordered_map<std::string, std::vector<std::string>> hints;
};
void to_json(json& j, const SubtitleFileV1& obj);
void from_json(const json& j, SubtitleFileV1& obj);

// These functions essentially convert to and from the V1/V2 formats to either load from disk, or
// persist to disk
//
// Returns the full file pair, then the base pair
GameSubtitlePackage read_json_files_v1(const GameSubtitleDefinitionFile& file_info);
SubtitleMetadataFileV1 dump_bank_meta_v1(const GameVersion game_version,
                                         std::shared_ptr<GameSubtitleBank> bank);
SubtitleFileV1 dump_bank_lines_v1(const GameVersion game_version,
                                  std::shared_ptr<GameSubtitleBank> bank);

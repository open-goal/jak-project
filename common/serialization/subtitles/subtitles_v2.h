#pragma once

#include "common/serialization/subtitles/subtitles.h"

struct SubtitleCutsceneLineMetadataV2 : SubtitleCutsceneLineMetadata {
  int frame_end;
  bool merge;

  // TODO
  /*bool operator<(const Subtitle2Line& other) const {
     return (start < other.start) || (start == other.start && end < other.end);
   }*/
};
void to_json(json& j, const SubtitleCutsceneLineMetadataV2& obj);
void from_json(const json& j, SubtitleCutsceneLineMetadataV2& obj);

struct SubtitleLineV2 : SubtitleCutsceneLineMetadataV2 {
  std::string text;
};

struct GameSubtitleSceneInfoV2 {
  bool is_scene;
  std::vector<SubtitleLineV2> m_lines;
};

#pragma once

struct SubtitleCutsceneLineMetadataV1 : SubtitleCutsceneLineMetadata {
  bool clear;
};
void to_json(json& j, const SubtitleCutsceneLineMetadataV1& obj);
void from_json(const json& j, SubtitleCutsceneLineMetadataV1& obj);

struct SubtitleLineV1 : SubtitleCutsceneLineMetadataV1 {
  std::string text;
};

struct GameSubtitleSceneInfoV1 {
  enum class SceneKind { Invalid = -1, Movie = 0, Hint = 1, HintNamed = 2 };

  GameSubtitleSceneInfoV1(SceneKind kind) : m_kind(kind) {}

  SceneKind m_kind;
  int m_id;
  std::vector<SubtitleLineV1> m_lines;
};

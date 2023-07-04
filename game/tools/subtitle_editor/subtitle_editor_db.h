#pragma once

#include <string>
#include <unordered_map>
#include <vector>

// Only really needed for Jak 1 as it had no built-in way to easily
// replay cutscenes
class Jak1SubtitleEditorDB {
 public:
  struct Entry {
    std::string entity_type;
    std::string process_name;
    std::string continue_name;
    std::vector<double> move_to;
    int delay_frames;
    std::string execute_code;
    bool move_first;
    std::vector<std::string> requirements;
  };

  std::unordered_map<std::string, Entry> m_db = {};

  void update();
};

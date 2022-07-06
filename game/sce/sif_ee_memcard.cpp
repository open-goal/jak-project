
#include "sif_ee_memcard.h"

#include <cstring>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/Serializer.h"

#include "game/sce/sif_ee.h"

namespace ee {
/*!
 * The actual data stored on the memory card.
 */
struct CardData {
  // each file has a name and data.
  struct File {
    std::vector<u8> data;
    bool is_directory = false;
  };
  // can be formatted or unformatted card.
  u32 is_formatted = 0;
  std::unordered_map<std::string, File> files;

  void save_to_file(const std::string& name);
  void load_from_file(const std::string& name);
};

void CardData::save_to_file(const std::string& name) {
  Serializer ser;
  ser.from_ptr(&is_formatted);
  ser.save<size_t>(files.size());
  for (auto& f : files) {
    ser.save_str(&f.first);
    ser.from_pod_vector(&f.second.data);
    ser.from_ptr(&f.second.is_directory);
  }
  auto result = ser.get_save_result();
  file_util::write_binary_file(name, result.first, result.second);
}

void CardData::load_from_file(const std::string& name) {
  auto raw_data = file_util::read_binary_file(name);
  Serializer ser(raw_data.data(), raw_data.size());

  ser.from_ptr(&is_formatted);
  files.clear();
  size_t file_count = ser.load<size_t>();
  for (size_t i = 0; i < file_count; i++) {
    auto file_name = ser.load_string();
    auto& file_entry = files[file_name];
    ser.from_pod_vector(&file_entry.data);
    ser.from_ptr(&file_entry.is_directory);
  }
  ASSERT(ser.get_load_finished());
}

std::string get_memory_card_path() {
  return file_util::get_file_path({"user", "memcard.bin"});
}

/*!
 * The actual memory card library state + current data.
 */
struct McState {
  s32 current_function = -1;  // -1 = nothing
  s32 current_function_result = 0;

  struct McFileHandle {
    std::string name;
    u32 fd = 0;
    s32 mode = 0;
    u32 seek = 0;
  };

  std::unordered_map<int, McFileHandle> handles;

  // TODO: we should load this data at startup from a memory card file, and save it at each write.
  CardData data;
  int next_fd = 1;
} g_mc_state;

int sceMcInit() {
  g_mc_state = McState();
  read_memory_card_from_file();
  return 1;
}

s32 sceMcMkdir(s32 port, s32 slot, const char* name) {
  ASSERT(port == 0);
  ASSERT(slot == 0);
  auto& file = g_mc_state.data.files[name];
  file.data.clear();
  file.is_directory = true;
  return sceMcResSucceed;
}

s32 sceMcSync(s32 mode, s32* cmd, s32* result) {
  // don't care about the mode, all memory card ops are instant.
  ASSERT(mode == 1 || mode == 0);
  if (g_mc_state.current_function == -1) {
    return sceMcExecIdle;
  } else {
    *cmd = g_mc_state.current_function;
    *result = g_mc_state.current_function_result;
    g_mc_state.current_function = -1;
    return sceMcExecFinish;
  }
}

s32 sceMcOpen(s32 port, s32 slot, const char* name, s32 mode) {
  ASSERT(port == 0);
  ASSERT(slot == 0);
  ASSERT(g_mc_state.current_function == -1);

  // add existing file, if it does not exist.
  auto existing_file = g_mc_state.data.files.find(name);
  if (existing_file == g_mc_state.data.files.end()) {
    ASSERT(mode & SCE_CREAT);
    g_mc_state.data.files[name] = {};
  }

  // create a handle.
  g_mc_state.current_function = sceMcFuncNoOpen;
  s32 fd = g_mc_state.next_fd++;
  McState::McFileHandle handle;
  handle.name = name;
  handle.fd = fd;
  handle.mode = mode;
  handle.seek = 0;
  g_mc_state.handles[fd] = handle;

  g_mc_state.current_function_result = fd;
  return sceMcResSucceed;
}

s32 sceMcWrite(s32 fd, const void* buff, s32 size) {
  ASSERT(g_mc_state.current_function == -1);

  ASSERT(size >= 0 && size < (1024 * 1024 * 1024));
  auto hand = g_mc_state.handles.find(fd);
  ASSERT(hand != g_mc_state.handles.end());  // make sure fd is valid
  ASSERT(hand->second.mode & SCE_WRONLY);    // make sure we're allowed to write

  const auto& file = g_mc_state.data.files.find(hand->second.name);
  ASSERT(file != g_mc_state.data.files.end());

  file->second.data.resize(size + hand->second.seek);
  memcpy(file->second.data.data() + hand->second.seek, buff, size);
  hand->second.seek += size;

  // TODO: save memcard data to a file.

  g_mc_state.current_function = sceMcFuncNoWrite;
  g_mc_state.current_function_result = size;
  return 0;
}

s32 sceMcClose(s32 fd) {
  ASSERT(g_mc_state.current_function == -1);
  auto hand = g_mc_state.handles.find(fd);
  ASSERT(hand != g_mc_state.handles.end());  // make sure fd is valid
  g_mc_state.handles.erase(fd);
  g_mc_state.current_function = sceMcFuncNoClose;
  g_mc_state.current_function_result = sceMcResSucceed;
  return 0;
}

s32 sceMcGetInfo(s32 port, s32 slot, s32* type, s32* free, s32* format) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(port == 0 || port == 1);
  ASSERT(slot == 0);

  if (port == 0) {
    if (type) {
      *type = sceMcTypePS2;
    }

    if (free) {
      *free = 2 * 1024;  // number of free 1 kB clusters
    }

    if (format) {
      *format = g_mc_state.data.is_formatted;
    }

    g_mc_state.current_function = sceMcFuncNoCardInfo;

    // technically this should return something else the first time you call this function after
    // changing cards.
    g_mc_state.current_function_result = sceMcResSucceed;
  } else {
    g_mc_state.current_function = sceMcFuncNoCardInfo;
    g_mc_state.current_function_result = -123;
  }

  return 0;
}

s32 sceMcFormat(s32 port, s32 slot) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(port == 0);
  ASSERT(slot == 0);
  g_mc_state.data.is_formatted = true;
  g_mc_state.current_function_result = sceMcResSucceed;
  g_mc_state.current_function = sceMcFuncNoFormat;
  return 0;
}

s32 sceMcUnformat(s32 port, s32 slot) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(port == 0);
  ASSERT(slot == 0);
  g_mc_state.data.is_formatted = false;
  g_mc_state.current_function_result = sceMcResSucceed;
  g_mc_state.current_function = sceMcFuncNoUnformat;
  return 0;
}

s32 sceMcDelete(s32 port, s32 slot, const char* name) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(port == 0);
  ASSERT(slot == 0);
  g_mc_state.current_function = sceMcFuncNoDelete;

  if (!g_mc_state.data.is_formatted) {
    g_mc_state.current_function_result = sceMcResNoFormat;
  } else {
    auto it = g_mc_state.data.files.find(name);
    if (it == g_mc_state.data.files.end()) {
      g_mc_state.current_function_result = sceMcResNoEntry;
    } else {
      // sometimes should be sceMcResNotEmpty, but doesn't matter.
      g_mc_state.current_function_result = sceMcResSucceed;
      g_mc_state.data.files.erase(it);
    }
  }

  return 0;
}

sceMcStDateTime make_fake_date_time() {
  sceMcStDateTime dt;
  dt.day = 1;
  dt.month = 22;
  dt.hour = 7;
  dt.min = 12;
  dt.year = 153;  // ??
  return dt;
}

s32 sceMcGetDir(s32 port, int slot, const char* name, u32 mode, s32 maxent, sceMcTblGetDir* table) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(port == 0);
  ASSERT(slot == 0);
  ASSERT(maxent == 1);
  ASSERT(mode == 0);
  ASSERT(g_mc_state.data.is_formatted);
  g_mc_state.current_function = sceMcFuncNoGetDir;

  auto file_it = g_mc_state.data.files.find(name);
  if (file_it == g_mc_state.data.files.end()) {
    g_mc_state.current_function_result = 0;
    return 0;
  } else {
    g_mc_state.current_function_result = 1;
    // ASSERT(strlen(name) < 32);
    strcpy(table[0].name, "blah");
    table[0].file_size = file_it->second.data.size();
    table[0].created = make_fake_date_time();
    table[0].modified = make_fake_date_time();
    return 0;
  }
}

s32 sceMcRead(s32 fd, void* buff, s32 size) {
  ASSERT(g_mc_state.current_function == -1);
  ASSERT(g_mc_state.data.is_formatted);
  auto it = g_mc_state.handles.find(fd);
  ASSERT(it != g_mc_state.handles.end());
  auto file_it = g_mc_state.data.files.find(it->second.name);
  // todo check read/write mode
  ASSERT(file_it != g_mc_state.data.files.end());
  ASSERT(size + it->second.seek <= file_it->second.data.size());

  memcpy(buff, file_it->second.data.data() + it->second.seek, size);
  it->second.seek += size;
  g_mc_state.current_function_result = size;
  g_mc_state.current_function = sceMcFuncNoRead;
  return 0;
}

void flush_memory_card_to_file() {
  file_util::create_dir_if_needed(file_util::get_file_path({"user"}));
  g_mc_state.data.save_to_file(get_memory_card_path());
}

void read_memory_card_from_file() {
  if (fs::exists(get_memory_card_path())) {
    g_mc_state.data.load_from_file(get_memory_card_path());
  }
}

}  // namespace ee

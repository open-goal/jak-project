#include <cstring>
#include "common/util/assert.h"
#include <cstdio>
#include <unordered_map>
#include "common/util/FileUtil.h"
#include "sif_ee.h"
#include "game/system/iop_thread.h"
#include "game/runtime.h"

namespace ee {

namespace {
::IOP* iop;
std::unordered_map<s32, FILE*> sce_fds;
}  // namespace

void LIBRARY_sceSif_register(::IOP* i) {
  iop = i;
}

void LIBRARY_INIT_sceSif() {
  iop = nullptr;
  for (auto& kv : sce_fds) {
    fclose(kv.second);
  }
  sce_fds.clear();
}
void sceSifInitRpc(unsigned int mode) {
  (void)mode;
}

int sceSifRebootIop(const char* imgfile) {
  (void)imgfile;
  return 1;
}

int sceSifSyncIop() {
  return 1;
}

void sceFsReset() {}

int sceSifLoadModule(const char* name, int arg_size, const char* args) {
  if (!strcmp(name, "cdrom0:\\\\DRIVERS\\\\OVERLORD.IRX;1") ||
      !strcmp(name, "host0:binee/overlord.irx")) {
    const char* src = args;
    char* dst = iop->overlord_arg_data;
    int cnt;
    iop->overlord_argv[0] = nullptr;
    for (cnt = 1; src - args < arg_size; cnt++) {
      auto len = strlen(src);
      memcpy(dst, src, len + 1);
      iop->overlord_argv[cnt] = dst;
      dst += len + 1;
      src += len + 1;
    }
    iop->overlord_argc = cnt;

    for (int i = 0; i < cnt; i++) {
      if (iop->overlord_argv[i])
        printf("arg %d : %s\n", i, iop->overlord_argv[i]);
    }
    iop->set_ee_main_mem(g_ee_main_mem);
    iop->send_status(IOP_Status::IOP_OVERLORD_INIT);
    iop->wait_for_overlord_init_finish();
  }

  return 1;
}

s32 sceSifCallRpc(sceSifClientData* bd,
                  u32 fno,
                  u32 mode,
                  void* send,
                  s32 ssize,
                  void* recv,
                  s32 rsize,
                  void* end_func,
                  void* end_para) {
  assert(!end_func);
  assert(!end_para);
  assert(mode == 1);  // async
  iop->kernel.sif_rpc(bd->rpcd.id, fno, mode, send, ssize, recv, rsize);
  iop->signal_run_iop();
  return 0;
}

s32 sceSifCheckStatRpc(sceSifRpcData* bd) {
  iop->signal_run_iop();
  return iop->kernel.sif_busy(bd->id);
}

s32 sceSifBindRpc(sceSifClientData* bd, u32 request, u32 mode) {
  assert(mode == 1);  // async
  bd->rpcd.id = request;
  bd->serve = (sceSifServeData*)1;
  return 0;
}

s32 sceOpen(const char* filename, s32 flag) {
  auto name = file_util::get_file_path({filename});
  FILE* fp = nullptr;
  switch (flag) {
    case SCE_RDONLY:
      fp = fopen(name.c_str(), "r");
      break;
    default:
      assert(false);
  }

  if (!fp) {
    printf("[SCE] sceOpen(%s) failed.\n", name.c_str());
    return -1;
  }

  s32 fp_idx = sce_fds.size() + 1;
  sce_fds[fp_idx] = fp;
  return fp_idx;
}

s32 sceClose(s32 fd) {
  if (fd < 0) {
    // todo, what should we really return?
    return 0;
  }

  auto kv = sce_fds.find(fd);
  if (kv != sce_fds.end()) {
    fclose(kv->second);
    sce_fds.erase(fd);
    return 0;
  } else {
    printf("[SCE] sceClose called on invalid fd\n");
    return 0;
  }
}

s32 sceRead(s32 fd, void* buf, s32 nbyte) {
  auto kv = sce_fds.find(fd);
  if (kv == sce_fds.end()) {
    return -1;
  } else {
    return fread(buf, 1, nbyte, kv->second);
  }
}

s32 sceWrite(s32 fd, const void* buf, s32 nbyte) {
  (void)fd;
  (void)buf;
  (void)nbyte;
  assert(false);
  return 0;
}

s32 sceLseek(s32 fd, s32 offset, s32 where) {
  auto kv = sce_fds.find(fd);
  if (kv == sce_fds.end()) {
    return -1;
  } else {
    switch (where) {
      case SCE_SEEK_CUR:
        fseek(kv->second, offset, SEEK_CUR);
        return ftell(kv->second);
      case SCE_SEEK_END:
        fseek(kv->second, offset, SEEK_END);
        return ftell(kv->second);
      case SCE_SEEK_SET:
        fseek(kv->second, offset, SEEK_SET);
        return ftell(kv->second);
      default:
        assert(false);
        return -1;
    }
  }
}

/*!
 * The actual data stored on the memory card.
 */
struct CardData {
  // each file has a name and data.
  struct File {
    std::string name;
    std::vector<u8> data;
  };
  // can be formatted or unformatted card.
  u32 is_formatted = 0;
  std::unordered_map<std::string, File> files;
};

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
  };

  std::unordered_map<int, McFileHandle> handles;

  // TODO: we should load this data at startup from a memory card file, and save it at each write.
  CardData data;
  int next_fd = 1;
} g_mc_state;

int sceMcInit() {
  g_mc_state = McState();
  return 1;
}

s32 sceMcMkdir(s32 port, s32 slot, const char* name) {
  assert(port == 0);
  assert(slot == 0);
  // TODO name
  (void)name;
  return sceMcResSucceed;
}

s32 sceMcSync(s32 mode, s32* cmd, s32* result) {
  // don't care about the mode, all memory card ops are instant.
  assert(mode == 1 || mode == 0);
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
  assert(port == 0);
  assert(slot == 0);
  assert(g_mc_state.current_function == -1);

  // add existing file, if it does not exist.
  auto existing_file = g_mc_state.data.files.find(name);
  if (existing_file == g_mc_state.data.files.end()) {
    assert(mode & SCE_CREAT);
    g_mc_state.data.files[name] = {};
  }

  // create a handle.
  g_mc_state.current_function = sceMcFuncNoOpen;
  s32 fd = g_mc_state.next_fd++;
  McState::McFileHandle handle;
  handle.name = name;
  handle.fd = fd;
  handle.mode = mode;
  g_mc_state.handles[fd] = handle;

  g_mc_state.current_function_result = fd;
  return 0;
}

s32 sceMcWrite(s32 fd, const void* buff, s32 size) {
  assert(g_mc_state.current_function == -1);

  assert(size >= 0 && size < (1024 * 1024 * 1024));
  auto hand = g_mc_state.handles.find(fd);
  assert(hand != g_mc_state.handles.end());  // make sure fd is valid
  assert(hand->second.mode & SCE_WRONLY);    // make sure we're allowed to write

  const auto& file = g_mc_state.data.files.find(hand->second.name);
  assert(file != g_mc_state.data.files.end());

  file->second.data.resize(size);
  memcpy(file->second.data.data(), buff, size);

  // TODO: save memcard data to a file.

  g_mc_state.current_function = sceMcFuncNoWrite;
  g_mc_state.current_function_result = size;
  return 0;
}

s32 sceMcClose(s32 fd) {
  assert(g_mc_state.current_function == -1);
  auto hand = g_mc_state.handles.find(fd);
  assert(hand != g_mc_state.handles.end());  // make sure fd is valid
  g_mc_state.handles.erase(fd);
  g_mc_state.current_function = sceMcFuncNoClose;
  g_mc_state.current_function_result = sceMcResSucceed;
  return 0;
}

s32 sceMcGetInfo(s32 port, s32 slot, s32* type, s32* free, s32* format) {
  assert(g_mc_state.current_function == -1);
  assert(port == 0);
  assert(slot == 0);
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
  return 0;
}

s32 sceMcFormat(s32 port, s32 slot) {
  assert(g_mc_state.current_function == -1);
  assert(port == 0);
  assert(slot == 0);
  g_mc_state.data.is_formatted = true;
  g_mc_state.current_function_result = sceMcResSucceed;
  g_mc_state.current_function = sceMcFuncNoFormat;
  return 0;
}

s32 sceMcUnformat(s32 port, s32 slot) {
  assert(g_mc_state.current_function == -1);
  assert(port == 0);
  assert(slot == 0);
  g_mc_state.data.is_formatted = false;
  g_mc_state.current_function_result = sceMcResSucceed;
  g_mc_state.current_function = sceMcFuncNoUnformat;
  return 0;
}

s32 sceMcDelete(s32 port, s32 slot, const char* name) {
  assert(g_mc_state.current_function == -1);
  assert(port == 0);
  assert(slot == 0);
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
}  // namespace ee
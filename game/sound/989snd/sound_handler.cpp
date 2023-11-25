#include "sound_handler.h"

#include "ame_handler.h"
#include "blocksound_handler.h"
#include "midi_handler.h"

namespace snd {

enum {
  SOUND_BLOCK = 1,
  SOUND_MIDI,
  SOUND_AME,
};

static constexpr SoundHandle MakeHandle(u8 tag, u8 idx, u16 sequence) {
  return ((tag & 0x1f) << 24) | (idx << 16) | sequence;
}

static constexpr u8 HandleType(SoundHandle handle) {
  return (handle >> 24) & 0x1f;
}

static constexpr u8 HandleIndex(SoundHandle handle) {
  return (handle >> 16) & 0xff;
}

static constexpr u16 HandleSequence(SoundHandle handle) {
  return handle & 0xffff;
}

template <typename T, u32 size, u8 tag>
class HandlerPool {
 public:
  HandlerPool() {
    int i = 0;
    for (auto& h : mHandlers) {
      h.ID = MakeHandle(tag, i, 0);
      h.Free = true;
      i++;
    }
  }

  T* GetPtrFromHandle(SoundHandle handle) {
    auto idx = HandleIndex(handle);
    if (idx >= size) {
      return nullptr;
    }

    auto& h = mHandlers[idx];
    if (h.Free == false && h.ID == handle) {
      return reinterpret_cast<T*>(&h.hnd);
    }

    return nullptr;
  }

  template <typename... Args>
  T* AllocateHandler(Args&&... args) {
    for (auto& h : mHandlers) {
      if (h.Free) {
        auto idx = HandleIndex(h.ID);
        auto sq = HandleSequence(h.ID);

        h.ID = MakeHandle(tag, idx, sq + 1);
        h.Free = false;

        auto p = new (&h.hnd) T(h.ID, args...);

        return p;
      }
    }

    return nullptr;
  }

  void FreeHandler(SoundHandle handle) {
    auto idx = HandleIndex(handle);
    if (idx >= size) {
      return;
    }

    auto& h = mHandlers[idx];
    if (h.Free == false && h.ID == handle) {
      auto* p = reinterpret_cast<T*>(&h.hnd);
      std::destroy_at(p);
      h.Free = true;
    }
  }

  T* Idx(std::size_t idx) {
    if (idx >= size) {
      return nullptr;
    }

    auto& h = mHandlers[idx];
    if (h.Free == false) {
      return reinterpret_cast<T*>(&h.hnd);
    }
  }

 private:
  struct HandlerEntry {
    SoundHandle ID{0};
    bool Free{true};
    typename std::aligned_storage<sizeof(T), alignof(T)>::type hnd;
  };

  std::array<HandlerEntry, size> mHandlers;
};

HandlerPool<BlockSoundHandler, 64, SOUND_BLOCK> gBlockSounds;
HandlerPool<MidiHandler, 32, SOUND_MIDI> gMidiSounds;
HandlerPool<AmeHandler, 4, SOUND_AME> gAmeSounds;

void SoundInit() {
  // TODO reset?
}

bool CheckInstanceLimit(SFXBlock::SFX& sfx, s32 sfx_vol, BlockSoundHandler** weakest_out) {
  s32 instances = 0;
  BlockSoundHandler* weakest = nullptr;

  if (!sfx.InstanceLimit) {
    return false;
  }

  if (sfx.Flags.instlimit_tick()) {
    lg::warn("unhandled tick instlimit");
  }

  for (int i = 0; i < 64; i++) {
    auto* s = gBlockSounds.Idx(i);
    if (s == nullptr) {
      continue;
    }

    if (&s->m_sfx == &sfx) {
      instances++;
      if (!weakest) {
        weakest = s;
      }

      if (sfx.Flags.instlimit_vol() && s->m_app_volume < weakest->m_app_volume) {
        weakest = s;
      }

      // if (sfx.Flags.instlimit_tick() && s->m_start_tick < weakest->m_start_tick) {
      //   weakest = s;
      // }
    }
  }

  if (instances > sfx.InstanceLimit) {
    lg::warn("instance limit exceeded {}", sfx.InstanceLimit);
    if (!weakest) {
      lg::warn("no weakest");
      return false;
    }

    if (sfx.Flags.instlimit_vol() && weakest->m_app_volume < sfx_vol) {
      *weakest_out = weakest;
      lg::warn("found weaker handler {} < {}", weakest->m_app_volume, sfx_vol);
      return true;
    }

    if (sfx.Flags.instlimit_vol()) {
      lg::warn("failed to find weaker handler {} < {}", weakest->m_app_volume, sfx_vol);
    }

    return false;
  }

  return true;
}

BlockSoundHandler* AllocBlockSound(SoundBank& bank, SFXBlock::SFX& sfx, s32 sfx_vol) {
  BlockSoundHandler* weakest = nullptr;

  if (sfx.Flags.has_instlimit()) {
    if (!CheckInstanceLimit(sfx, sfx_vol, &weakest)) {
      return nullptr;
    }

    if (weakest) {
      weakest->Stop();
    }
  }

  return gBlockSounds.AllocateHandler(bank, sfx);
}

MidiHandler* AllocMidiSound(Midi* block,
                            MusicBank::MIDISound& sound,
                            s32 vol,
                            s32 pan,
                            SoundBank& bank) {
  return gMidiSounds.AllocateHandler(block, sound, vol, pan, bank);
}

AmeHandler* AllocAmeSound(MultiMidi* block,
                          MusicBank::MIDISound& sound,
                          s32 vol,
                          s32 pan,
                          SoundBank& bank) {
  return gAmeSounds.AllocateHandler(block, sound, vol, pan, bank);
}

void FreeSound(SoundHandler* handler) {
  auto handle = handler->Handle();
  u8 type = HandleType(handle);
  switch (type) {
    case SOUND_BLOCK:
      gBlockSounds.FreeHandler(handle);
      break;
    case SOUND_MIDI:
      gMidiSounds.FreeHandler(handle);
      break;
    case SOUND_AME:
      gAmeSounds.FreeHandler(handle);
      break;
    default:
      lg::die("Unknown sound type (messed up sound handle) {:x}", handle);
      break;
  }
}

SoundHandler* GetSound(SoundHandle handle) {
  if (handle == 0) {
    return nullptr;
  }

  u8 type = HandleType(handle);
  switch (type) {
    case SOUND_BLOCK:
      return gBlockSounds.GetPtrFromHandle(handle);
      break;
    case SOUND_MIDI:
      return gMidiSounds.GetPtrFromHandle(handle);
      break;
    case SOUND_AME:
      return gAmeSounds.GetPtrFromHandle(handle);
      break;
    default:
      lg::die("Unknown sound type (messed up sound handle) {:x}", handle);
      break;
  }

  return nullptr;
}

}  // namespace snd

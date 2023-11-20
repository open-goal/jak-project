#include "lfo.h"

#include <array>

#include "blocksound_handler.h"

namespace snd {
#include "lfo_sine.c.inc"

void LFOTracker::Init() {
  if (m_type == LFOType::RAND) {
    m_state_hold1 = -(rand() & 0x7fff) * (rand() & 1);
    m_state_hold2 = 1;
  }

  CalcDepth();
  Tick();
}

void LFOTracker::CalcDepth() {
  if (m_target == LFOTarget::VOLUME) {
    m_range = (m_handler.m_sfx.Vol * m_depth) >> 10;
  }
  if (m_target == LFOTarget::PAN) {
    m_range = (180 * m_depth) >> 10;
  }
  if (m_target == LFOTarget::PMOD) {
    m_range = (6096 * m_depth) >> 10;
  }
  if (m_target == LFOTarget::PBEND) {
    m_range = (0x7fff * m_depth) >> 10;
  }

  m_last_lfo = 0;
}

void LFOTracker::Tick() {
  m_tick++;

  if (m_target == LFOTarget::NONE || (m_tick & 1) == 0) {
    return;
  }

  switch (m_target) {
    case LFOTarget::VOLUME: {
      s32 vol = (m_range * (GetLFO(2) - 0x7fff)) >> 16;
      if (m_handler.m_lfo_volume != vol) {
        m_handler.m_lfo_volume = vol;
        m_handler.SetVolPan(VOLUME_DONT_CHANGE, PAN_DONT_CHANGE);
      }
    } break;
    case LFOTarget::PAN: {
      s32 pan = (m_range * GetLFO(2)) >> 15;
      if (m_handler.m_lfo_pan != pan) {
        m_handler.m_lfo_pan = pan;
        m_handler.SetVolPan(VOLUME_DONT_CHANGE, PAN_DONT_CHANGE);
      }
    } break;
    case LFOTarget::PMOD: {
      s32 pm = (GetLFO(2) * m_range) >> 15;
      if (m_handler.m_lfo_pm != pm) {
        m_handler.m_lfo_pm = pm;
        m_handler.UpdatePitch();
      }
    } break;
    case LFOTarget::PBEND: {
      s32 pb = (GetLFO(2) * m_range) >> 15;
      if (m_handler.m_lfo_pb != pb) {
        m_handler.m_lfo_pb = pb;
        m_handler.UpdatePitch();
      }
    } break;
    case LFOTarget::UNK1: {
    } break;
    case LFOTarget::UNK2: {
    } break;
    default:
      break;
  }
}

s32 LFOTracker::GetLFO(s32 step_mult) {
  s32 step = m_next_step >> 16;
  m_next_step += step_mult * m_step_size;
  if (m_next_step > 0x7ffffff) {
    m_next_step -= 0x8000000;
  }

  s32 ret = 0;

  switch (m_type) {
    case LFOType::OFF:
      ret = 0;
      break;
    case LFOType::SINE:
      ret = gLFO_sine.at(step);
      break;
    case LFOType::SQUARE:
      if (step >= m_state_hold1) {
        ret = -32767;
      } else {
        ret = 32767;
      }
      break;
    case LFOType::TRIANGLE:
      if (step < 512) {
        ret = 0x7fff * step / 512;
      } else if (step >= 1536) {
        ret = 0x7fff * (step - 1536) / 512 - 0x7fff;
      } else {
        ret = 0x7fff - 65534 * (step - 512) / 1024;
      }
      break;
    case LFOType::SAW:
      if (step >= 1024) {
        ret = 0x7fff * (step - 1024) / 1024 - 0x7fff;
      } else {
        ret = 0x7fff * step / 1023;
      }
      break;
    case LFOType::RAND:
      if (step >= 1024 && m_state_hold2 == 1) {
        m_state_hold2 = 0;
        m_state_hold1 = 2 * ((rand() & 0x7fff) - 0x3fff);
      } else if (step < 1024 && m_state_hold2 == 0) {
        m_state_hold2 = 1;
        m_state_hold1 = -(rand() & 0x7fff) * (rand() & 1);
      }
      ret = m_state_hold1;
      break;
  }

  if ((m_setup_flags & 1) != 0) {
    ret = -ret;
  }

  return ret;
}

}  // namespace snd

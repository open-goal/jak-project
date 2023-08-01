#include "lfo.h"

#include <array>

#include "blocksound_handler.h"

namespace snd {
#include "lfo_sine.c.inc"

void LFOTracker::init() {
  if (m_type == lfo_type::RAND) {
    m_state_hold1 = -(rand() & 0x7fff) * (rand() & 1);
    m_state_hold2 = 1;
  }

  calc_depth();
  tick();
}

void LFOTracker::calc_depth() {
  if (m_target == lfo_target::VOLUME) {
    m_range = (m_handler.m_sfx.d.Vol * m_depth) >> 10;
  }
  if (m_target == lfo_target::PAN) {
    m_range = (180 * m_depth) >> 10;
  }
  if (m_target == lfo_target::PMOD) {
    m_range = (6096 * m_depth) >> 10;
  }
  if (m_target == lfo_target::PBEND) {
    m_range = (0x7fff * m_depth) >> 10;
  }

  m_last_lfo = 0;
}

void LFOTracker::tick() {
  m_tick++;

  if (m_target == lfo_target::NONE || (m_tick & 1) == 0) {
    return;
  }

  switch (m_target) {
    case lfo_target::VOLUME: {
      s32 vol = (m_range * (get_lfo(2) - 0x7fff)) >> 16;
      if (m_handler.m_lfo_volume != vol) {
        m_handler.m_lfo_volume = vol;
        m_handler.set_vol_pan(VOLUME_DONT_CHANGE, PAN_DONT_CHANGE);
      }
    } break;
    case lfo_target::PAN: {
      s32 pan = (m_range * get_lfo(2)) >> 15;
      if (m_handler.m_lfo_pan != pan) {
        m_handler.m_lfo_pan = pan;
        m_handler.set_vol_pan(VOLUME_DONT_CHANGE, PAN_DONT_CHANGE);
      }
    } break;
    case lfo_target::PMOD: {
      s32 pm = (get_lfo(2) * m_range) >> 15;
      if (m_handler.m_lfo_pm != pm) {
        m_handler.m_lfo_pm = pm;
        m_handler.update_pitch();
      }
    } break;
    case lfo_target::PBEND: {
      s32 pb = (get_lfo(2) * m_range) >> 15;
      if (m_handler.m_lfo_pb != pb) {
        m_handler.m_lfo_pb = pb;
        m_handler.update_pitch();
      }
    } break;
    case lfo_target::UNK1: {
    } break;
    case lfo_target::UNK2: {
    } break;
    default:
      break;
  }
}

s32 LFOTracker::get_lfo(s32 step_mult) {
  s32 step = m_next_step >> 16;
  m_next_step += step_mult * m_step_size;
  if (m_next_step > 0x7ffffff) {
    m_next_step -= 0x8000000;
  }

  s32 ret = 0;

  switch (m_type) {
    case lfo_type::OFF:
      ret = 0;
      break;
    case lfo_type::SINE:
      ret = gLFO_sine.at(step);
      break;
    case lfo_type::SQUARE:
      if (step >= m_state_hold1) {
        ret = -32767;
      } else {
        ret = 32767;
      }
      break;
    case lfo_type::TRIANGLE:
      if (step < 512) {
        ret = 0x7fff * step / 512;
      } else if (step >= 1536) {
        ret = 0x7fff * (step - 1536) / 512 - 0x7fff;
      } else {
        ret = 0x7fff - 65534 * (step - 512) / 1024;
      }
      break;
    case lfo_type::SAW:
      if (step >= 1024) {
        ret = 0x7fff * (step - 1024) / 1024 - 0x7fff;
      } else {
        ret = 0x7fff * step / 1023;
      }
      break;
    case lfo_type::RAND:
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

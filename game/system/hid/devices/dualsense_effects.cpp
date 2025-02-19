/*
 * MIT License
 *
 * Copyright (c) 2021-2022 John "Nielk1" Klein
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * https://gist.github.com/Nielk1/6d54cc2c00d2201ccb8c2720ad7538db
 */

#include "dualsense_effects.h"

#include <vector>

#include "array"

#include "common/common_types.h"
#include "common/log/log.h"

namespace dualsense_effects {

std::array<u8, 11> trigger_effect_off() {
  std::array<u8, 11> effect_data;
  effect_data[0] = (u8)TriggerEffectType::Off;
  effect_data[1] = 0x00;
  effect_data[2] = 0x00;
  effect_data[3] = 0x00;
  effect_data[4] = 0x00;
  effect_data[5] = 0x00;
  effect_data[6] = 0x00;
  effect_data[7] = 0x00;
  effect_data[8] = 0x00;
  effect_data[9] = 0x00;
  effect_data[10] = 0x00;
  return effect_data;
}

std::array<u8, 11> trigger_effect_feedback(u8 position, u8 strength) {
  if (position > 9) {
    lg::error("Invalid argument for feedback trigger effect, position must be <= 9");
    return trigger_effect_off();
  }
  if (strength > 8) {
    lg::error("Invalid argument for feedback trigger effect, strength must be <= 8");
    return trigger_effect_off();
  }
  if (strength > 0) {
    std::array<u8, 11> effect_data;
    u8 forceValue = (u8)((strength - 1) & 0x07);
    u32 forceZones = 0;
    u16 activeZones = 0;
    for (int i = position; i < 10; i++) {
      forceZones |= (u32)(forceValue << (3 * i));
      activeZones |= (u16)(1 << i);
    }

    effect_data[0] = (u8)TriggerEffectType::Feedback;
    effect_data[1] = (u8)((activeZones >> 0) & 0xff);
    effect_data[2] = (u8)((activeZones >> 8) & 0xff);
    effect_data[3] = (u8)((forceZones >> 0) & 0xff);
    effect_data[4] = (u8)((forceZones >> 8) & 0xff);
    effect_data[5] = (u8)((forceZones >> 16) & 0xff);
    effect_data[6] = (u8)((forceZones >> 24) & 0xff);
    effect_data[7] = 0x00;  // (byte)((forceZones >> 32) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[8] = 0x00;  // (byte)((forceZones >> 40) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[9] = 0x00;
    effect_data[10] = 0x00;
    return effect_data;
  }
  return trigger_effect_off();
}

std::array<u8, 11> trigger_effect_weapon(u8 start_position, u8 end_position, u8 strength) {
  if (start_position > 7 || start_position < 2) {
    lg::error("Invalid argument for weapon trigger effect, start_position must be <= 7 and >= 2");
    return trigger_effect_off();
  }
  if (end_position > 8) {
    lg::error("Invalid argument for weapon trigger effect, end_position must be <= 8");
    return trigger_effect_off();
  }
  if (end_position <= start_position) {
    lg::error("Invalid argument for weapon trigger effect, end_position must be > start_position");
    return trigger_effect_off();
  }
  if (strength > 8) {
    lg::error("Invalid argument for weapon trigger effect, strength must be <= 8");
    return trigger_effect_off();
  }
  if (strength > 0) {
    std::array<u8, 11> effect_data;
    u16 startAndStopZones = (u16)((1 << start_position) | (1 << end_position));

    effect_data[0] = (u8)TriggerEffectType::Weapon;
    effect_data[1] = (u8)((startAndStopZones >> 0) & 0xff);
    effect_data[2] = (u8)((startAndStopZones >> 8) & 0xff);
    effect_data[3] = (u8)(strength - 1);  // this is actually packed into 3 bits, but since it's
                                          // only one why bother with the fancy code?
    effect_data[4] = 0x00;
    effect_data[5] = 0x00;
    effect_data[6] = 0x00;
    effect_data[7] = 0x00;
    effect_data[8] = 0x00;
    effect_data[9] = 0x00;
    effect_data[10] = 0x00;
    return effect_data;
  }
  return trigger_effect_off();
}

std::array<u8, 11> trigger_effect_vibration(u8 position, u8 amplitude, u8 frequency) {
  if (position > 9) {
    lg::error("Invalid argument for vibration trigger effect, position must be <= 9");
    return trigger_effect_off();
  }
  if (amplitude > 8) {
    lg::error("Invalid argument for vibration trigger effect, amplitude must be <= 8");
    return trigger_effect_off();
  }
  if (amplitude > 0 && frequency > 0) {
    u8 strengthValue = (u8)((amplitude - 1) & 0x07);
    u32 amplitudeZones = 0;
    u16 activeZones = 0;
    for (int i = position; i < 10; i++) {
      amplitudeZones |= (u32)(strengthValue << (3 * i));
      activeZones |= (u16)(1 << i);
    }
    std::array<u8, 11> effect_data;
    effect_data[0] = (u8)TriggerEffectType::Vibration;
    effect_data[1] = (u8)((activeZones >> 0) & 0xff);
    effect_data[2] = (u8)((activeZones >> 8) & 0xff);
    effect_data[3] = (u8)((amplitudeZones >> 0) & 0xff);
    effect_data[4] = (u8)((amplitudeZones >> 8) & 0xff);
    effect_data[5] = (u8)((amplitudeZones >> 16) & 0xff);
    effect_data[6] = (u8)((amplitudeZones >> 24) & 0xff);
    effect_data[7] = 0x00;  // (u8)((strengthZones >> 32) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[8] = 0x00;  // (u8)((strengthZones >> 40) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[9] = frequency;
    effect_data[10] = 0x00;
    return effect_data;
  }
  return trigger_effect_off();
}

std::array<u8, 11> trigger_effect_multiple_position_feedback(const std::vector<u8>& strength) {
  if (strength.size() != 10) {
    lg::error(
        "Invalid argument for multiple_position_feedback trigger effect, strength array must be "
        "length 10");
    return trigger_effect_off();
  }

  // Check to see if any of the strength values are actually above 0
  bool return_off = true;
  for (const auto& strength : strength) {
    if (strength > 0) {
      return_off = false;
      break;
    }
  }

  if (return_off) {
    return trigger_effect_off();
  }

  u32 forceZones = 0;
  u16 activeZones = 0;
  for (int i = 0; i < 10; i++) {
    if (strength[i] > 0) {
      u8 forceValue = (u8)((strength[i] - 1) & 0x07);
      forceZones |= (u32)(forceValue << (3 * i));
      activeZones |= (u16)(1 << i);
    }
  }
  std::array<u8, 11> effect_data;
  effect_data[0] = (u8)TriggerEffectType::Feedback;
  effect_data[1] = (u8)((activeZones >> 0) & 0xff);
  effect_data[2] = (u8)((activeZones >> 8) & 0xff);
  effect_data[3] = (u8)((forceZones >> 0) & 0xff);
  effect_data[4] = (u8)((forceZones >> 8) & 0xff);
  effect_data[5] = (u8)((forceZones >> 16) & 0xff);
  effect_data[6] = (u8)((forceZones >> 24) & 0xff);
  effect_data[7] = 0x00;  // (byte)((forceZones >> 32) & 0xff); // need 64bit for this, but we
                          // already have enough space
  effect_data[8] = 0x00;  // (byte)((forceZones >> 40) & 0xff); // need 64bit for this, but we
                          // already have enough space
  effect_data[9] = 0x00;
  effect_data[10] = 0x00;
  return effect_data;
}

std::array<u8, 11> trigger_effect_slope_feedback(u8 start_position,
                                                 u8 end_position,
                                                 u8 start_strength,
                                                 u8 end_strength) {
  if (start_position > 8 || start_position < 0) {
    lg::error(
        "Invalid argument for slope_feedback trigger effect, start_position must be <= 8 and > 0");
    return trigger_effect_off();
  }
  if (end_position > 9) {
    lg::error("Invalid argument for slope_feedback trigger effect, end_position must be <= 9");
    return trigger_effect_off();
  }
  if (end_position <= start_position) {
    lg::error(
        "Invalid argument for slope_feedback trigger effect, end_position must be > "
        "start_position");
    return trigger_effect_off();
  }
  if (start_strength > 8) {
    lg::error("Invalid argument for slope_feedback trigger effect, start_strength must be >= 8");
    return trigger_effect_off();
  }
  if (start_strength < 1) {
    lg::error("Invalid argument for slope_feedback trigger effect, start_strength must be >= 1");
    return trigger_effect_off();
  }
  if (end_strength > 8) {
    lg::error("Invalid argument for slope_feedback trigger effect, end_strength must be <= 8");
    return trigger_effect_off();
  }
  if (end_strength < 1) {
    lg::error("Invalid argument for slope_feedback trigger effect, end_strength must be >= 1");
    return trigger_effect_off();
  }

  std::vector<u8> strength;
  float slope = 1.0f * (end_strength - start_strength) / (end_position - start_position);
  for (int i = (int)start_position; i < 10; i++)
    if (i <= end_position)
      strength[i] = (u8)round(start_strength + slope * (i - start_position));
    else
      strength[i] = end_strength;

  return trigger_effect_multiple_position_feedback(strength);
}

std::array<u8, 11> trigger_effect_multiple_position_vibrate(u8 frequency,
                                                            const std::vector<u8>& amplitude) {
  if (amplitude.size() != 10) {
    lg::error("Invalid argument for slope_feedback trigger effect, amplitude must be length 10");
    return trigger_effect_off();
  }

  // Check to see if any of the amplitude values are actually above 0
  bool return_off = true;
  for (const auto& amplitude : amplitude) {
    if (amplitude > 0) {
      return_off = false;
      break;
    }
  }

  if (return_off) {
    return trigger_effect_off();
  }

  if (frequency > 0) {
    u32 strengthZones = 0;
    u16 activeZones = 0;
    for (int i = 0; i < 10; i++) {
      if (amplitude[i] > 0) {
        u8 strengthValue = (u8)((amplitude[i] - 1) & 0x07);
        strengthZones |= (u32)(strengthValue << (3 * i));
        activeZones |= (u16)(1 << i);
      }
    }

    std::array<u8, 11> effect_data;
    effect_data[0] = (u8)TriggerEffectType::Vibration;
    effect_data[1] = (u8)((activeZones >> 0) & 0xff);
    effect_data[2] = (u8)((activeZones >> 8) & 0xff);
    effect_data[3] = (u8)((strengthZones >> 0) & 0xff);
    effect_data[4] = (u8)((strengthZones >> 8) & 0xff);
    effect_data[5] = (u8)((strengthZones >> 16) & 0xff);
    effect_data[6] = (u8)((strengthZones >> 24) & 0xff);
    effect_data[7] = 0x00;  // (byte)((forceZones >> 32) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[8] = 0x00;  // (byte)((forceZones >> 40) & 0xff); // need 64bit for this, but we
                            // already have enough space
    effect_data[9] = frequency;
    effect_data[10] = 0x00;
    return effect_data;
  }
  return trigger_effect_off();
}

};  // namespace dualsense_effects

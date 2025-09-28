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

#pragma once

#include <array>
#include <vector>

#include "common/common_types.h"

namespace dualsense_effects {

enum class TriggerEffectOption { LEFT = 0, RIGHT = 1, BOTH = 2 };

struct DS5EffectsState_t {
  u8 ucEnableBits1;              /* 0 */
  u8 ucEnableBits2;              /* 1 */
  u8 ucRumbleRight;              /* 2 */
  u8 ucRumbleLeft;               /* 3 */
  u8 ucHeadphoneVolume;          /* 4 */
  u8 ucSpeakerVolume;            /* 5 */
  u8 ucMicrophoneVolume;         /* 6 */
  u8 ucAudioEnableBits;          /* 7 */
  u8 ucMicLightMode;             /* 8 */
  u8 ucAudioMuteBits;            /* 9 */
  u8 rgucRightTriggerEffect[11]; /* 10 */
  u8 rgucLeftTriggerEffect[11];  /* 21 */
  u8 rgucUnknown1[6];            /* 32 */
  u8 ucLedFlags;                 /* 38 */
  u8 rgucUnknown2[2];            /* 39 */
  u8 ucLedAnim;                  /* 41 */
  u8 ucLedBrightness;            /* 42 */
  u8 ucPadLights;                /* 43 */
  u8 ucLedRed;                   /* 44 */
  u8 ucLedGreen;                 /* 45 */
  u8 ucLedBlue;                  /* 46 */
};

enum TriggerEffectType {
  // Offically recognized modes
  // These are 100% safe and are the only effects that modify the trigger status nybble
  Off = 0x05,        // 00 00 0 101
  Feedback = 0x21,   // 00 10 0 001
  Weapon = 0x25,     // 00 10 0 101
  Vibration = 0x26,  // 00 10 0 110
};

/// <summary>
/// Turn the trigger effect off and return the trigger stop to the neutral position.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <returns>The success of the effect
/// write.</returns>
std::array<u8, 11> trigger_effect_off();

/// <summary>
/// Trigger will resist movement beyond the start position.
/// The trigger status nybble will report 0 before the effect and 1 when in the effect.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <param name="position">The starting zone of the trigger effect. Must be between 0 and 9
/// inclusive.</param>
///
/// <param name="strength">The force of the resistance. Must be between 0 and 8 inclusive.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_feedback(u8 position, u8 strength);

/// <summary>
/// Trigger will resist movement beyond the start position until the end position.
/// The trigger status nybble will report 0 before the effect and 1 when in the effect,
/// and 2 after until again before the start position.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <param name="start_position">The starting zone of the trigger effect. Must be between 2 and 7
/// inclusive.</param>
///
/// <param name="end_position">The ending zone of the trigger effect. Must be between <paramref
/// name="start_position"/>+1 and 8 inclusive.</param>
///
/// <param name="strength">The force of the resistance. Must be between 0 and 8 inclusive.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_weapon(u8 start_position, u8 end_position, u8 strength);

/// <summary>
/// Trigger will vibrate with the input amplitude and frequency beyond the start position.
/// The trigger status nybble will report 0 before the effect and 1 when in the effect.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <param name="position">The starting zone of the trigger effect. Must be between 0 and 9
/// inclusive.</param>
///
/// <param name="amplitude">Strength of the automatic cycling action. Must be between 0 and 8
/// inclusive.</param>
///
/// <param name="frequency">Frequency of the automatic cycling action in hertz.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_vibration(u8 position, u8 amplitude, u8 frequency);

/// <summary>
/// Trigger will resist movement at varrying strengths in 10 regions.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <seealso cref="Feedback(byte[], int, byte, byte)"/>
/// <param name="strength">Array of 10 resistance values for zones 0 through 9. Must be between 0
/// and 8 inclusive.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_multiple_position_feedback(const std::vector<u8>& strength);

/// <summary>
/// Trigger will resist movement at a linear range of strengths.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <seealso cref="Feedback(byte[], int, byte, byte)"/>
/// <param name="startPosition">The starting zone of the trigger effect. Must be between 0 and 8
/// inclusive.</param>
///
/// <param name="endPosition">The ending zone of the trigger effect. Must be between <paramref
/// name="startPosition"/>+1 and 9 inclusive.</param>
///
/// <param name="startStrength">The force of the resistance at the start. Must be between 1 and 8
/// inclusive.</param>
///
/// <param name="endStrength">The force of the resistance at the end. Must be between 1 and 8
/// inclusive.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_slope_feedback(u8 start_position,
                                                 u8 end_position,
                                                 u8 start_strength,
                                                 u8 end_strength);

/// <summary>
/// Trigger will vibrate movement at varrying amplitudes and one frequency in 10 regions.
/// This is an offical effect and is expected to be present in future DualSense firmware.
/// </summary>
/// <remarks>
/// Note this factory's results may not perform as expected.
/// </remarks>
/// <seealso cref="Vibration(byte[], int, byte, byte, byte)"/>
/// <param name="amplitude">Array of 10 strength values for zones 0 through 9. Must be between 0 and
/// 8 inclusive.</param>
///
/// <param name="frequency">Frequency of the automatic cycling action in hertz.</param>
///
/// <returns>The success of the effect write.</returns>
std::array<u8, 11> trigger_effect_multiple_position_vibrate(u8 frequency,
                                                            const std::vector<u8>& amplitude);

};  // namespace dualsense_effects

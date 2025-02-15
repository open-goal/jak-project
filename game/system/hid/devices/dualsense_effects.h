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

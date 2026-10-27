#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/BinaryReader.h"
#include "common/util/FileUtil.h"

// The header data for a simple wave file
struct WaveFileHeader {
  // wave file header
  char chunk_id[4];
  s32 chunk_size;
  char format[4];

  // format chunk
  char subchunk1_id[4];
  s32 subchunk1_size;
  s16 aud_format;
  s16 num_channels;
  s32 sample_rate;
  s32 byte_rate;
  s16 block_align;
  s16 bits_per_sample;

  // data chunk
  char subchunk2_id[4];
  s32 subchunk2_size;
};

void write_wave_file(const std::vector<s16>& left_samples,
                     const std::vector<s16>& right_samples,
                     s32 sample_rate,
                     const fs::path& name);

std::pair<std::vector<s16>, std::vector<s16>> decode_adpcm(BinaryReader& reader,
                                                           const bool mono,
                                                           const u32 version);

/*!
 * Encode 16-bit samples as PS-ADPCM. The output is a sequence of 16-byte blocks (28 samples each,
 * the last block is zero padded) with all flag bytes set to 0. The filter and shift of every block
 * are chosen to minimize the squared error against the input.
 */
std::vector<u8> encode_adpcm(const std::vector<s16>& samples);

struct WaveData {
  std::vector<s16> left_samples;
  std::vector<s16> right_samples;  // empty for mono
  s32 sample_rate = 0;
};

/*!
 * Read a 16-bit PCM wave file (mono or stereo). Returns false and sets error on failure.
 */
bool read_wave_file(const fs::path& name, WaveData* out, std::string* error);

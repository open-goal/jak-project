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

void write_wave_file_mono(const std::vector<s16>& samples, s32 sample_rate, const fs::path& name);

std::vector<s16> decode_adpcm(BinaryReader& reader);

std::vector<u8> encode_adpcm(const std::vector<s16>& samples);

#include "audio_formats.h"

#include "common/log/log.h"
#include "common/util/BinaryWriter.h"

#include "third-party/fmt/core.h"

/*!
 * Write a wave file from a vector of samples.
 */
void write_wave_file_mono(const std::vector<s16>& samples, s32 sample_rate, const fs::path& name) {
  WaveFileHeader header;
  memcpy(header.chunk_id, "RIFF", 4);
  header.chunk_size = 36 + samples.size() * sizeof(s16);
  memcpy(header.format, "WAVE", 4);

  // now the format
  memcpy(header.subchunk1_id, "fmt ", 4);
  header.subchunk1_size = 16;
  header.aud_format = 1;
  header.num_channels = 1;  // mono
  header.sample_rate = sample_rate;
  header.byte_rate = sample_rate * header.num_channels * sizeof(s16);
  header.block_align = header.num_channels * sizeof(s16);
  header.bits_per_sample = 16;

  memcpy(header.subchunk2_id, "data", 4);
  header.subchunk2_size = samples.size() * sizeof(s16);

  BinaryWriter writer;
  writer.add(header);

  for (auto& samp : samples) {
    writer.add(samp);
  }

  writer.write_to_file(name);
}

std::vector<s16> decode_adpcm(BinaryReader& reader) {
  std::vector<s16> decoded_samples;
  s32 sample_prev[2] = {0, 0};
  constexpr s32 f1[5] = {0, 60, 115, 98, 122};
  constexpr s32 f2[5] = {0, 0, -52, -55, -60};

  [[maybe_unused]] int block_idx = 0;
  while (true) {
    if (!reader.bytes_left()) {
      break;
    }
    u8 shift_filter = reader.read<u8>();
    u8 flags = reader.read<u8>();
    u8 shift = shift_filter & 0b1111;
    u8 filter = shift_filter >> 4;

    if (shift > 12) {
      ASSERT(false);
    }

    if (filter > 4) {
      ASSERT(false);
    }

    if (flags == 7) {
      break;
    }

    u8 input_buffer[14];

    for (int i = 0; i < 14; i++) {
      input_buffer[i] = reader.read<u8>();
    }

    for (int i = 0; i < 28; i++) {
      int16_t nibble = input_buffer[i / 2];
      if (i % 2 == 0) {
        nibble = (nibble & 0x0f);
      } else {
        nibble = (nibble & 0xf0) >> 4;
      }

      s32 sample = (s32)(s16)(nibble << 12);
      sample >>= shift;
      sample += (sample_prev[0] * f1[filter] + sample_prev[1] * f2[filter] + 32) / 64;

      if (sample > 0x7fff) {
        sample = 0x7fff;
      }

      if (sample < -0x8000) {
        sample = -0x8000;
      }

      sample_prev[1] = sample_prev[0];
      sample_prev[0] = sample;

      decoded_samples.push_back(sample);
    }
    block_idx++;
  }

  return decoded_samples;
}

// I attempted to write an encoder below, which works, but has some limitations.
// - In some cases we can't recover the original data exactly because the decode saturates the
//   the output to fit in a signed 16-bit integer.
// - There are some cases when there are multiple ways to encode the same data.
//   The break_filter_ties function attempts to handle this, but doesn't work 100% of the time.

template <typename T>
T saturate(T in, T minimum, T maximum) {
  if (in < minimum) {
    return minimum;
  }
  if (in > maximum) {
    return maximum;
  }
  return in;
}

constexpr int SAMPLES_PER_BLOCK = 28;

void encode_block_with_filter(int filter_idx,
                              const s16* samples_in,
                              s32* out,
                              const s32* prev_samples_in) {
  constexpr s32 f1[5] = {0, 60, 115, 98, 122};
  constexpr s32 f2[5] = {0, 0, -52, -55, -60};
  s32 prev_samples[2] = {prev_samples_in[0], prev_samples_in[1]};

  for (int sample_idx = 0; sample_idx < SAMPLES_PER_BLOCK; sample_idx++) {
    s32 sample = samples_in[sample_idx];
    s32 delta =
        sample - (prev_samples[0] * f1[filter_idx] + prev_samples[1] * f2[filter_idx] + 32) / 64;
    out[sample_idx] = delta;
    prev_samples[1] = prev_samples[0];
    prev_samples[0] = sample;
  }
}

int get_shift_error(int shift, const s32* samples, bool /*debug*/) {
  int result = 0;

  for (int sample_idx = 0; sample_idx < SAMPLES_PER_BLOCK; sample_idx++) {
    int left_shift = 32 - (12 + 4 - shift);
    ASSERT(left_shift >= 0);
    s32 sample_left = samples[sample_idx] << left_shift;
    s32 sample_right = sample_left >> (32 - 4);
    s32 sample_compressed = sample_right << (12 - shift);

    s32 err = std::abs(sample_compressed - samples[sample_idx]);

    result += err;
  }
  return result;
}

int get_max_bits(s32 value) {
  int result = 0;
  if (value >= 0) {
    int last = 1;
    while (value) {
      result++;
      last = value & 1;
      value >>= 1;
    }
    if (last) {
      result++;
    }
  } else {
    int last = 0;
    while (value != -1) {
      result++;
      last = value & 1;
      value >>= 1;
    }
    if (!last) {
      result++;
    }
  }
  return result;
}

int break_filter_ties(s32* errors, s32* filter_shifts) {
  s32 best_error = INT32_MAX;

  for (int filter_idx = 0; filter_idx < 5; filter_idx++) {
    if (errors[filter_idx] < best_error) {
      best_error = errors[filter_idx];
    }
  }

  s32 best_shift = INT32_MAX;
  int best_filter = -1;
  for (int filter_idx = 5; filter_idx-- > 0;) {
    if (errors[filter_idx] == best_error) {
      if (filter_shifts[filter_idx] <= best_shift) {
        best_shift = filter_shifts[filter_idx];
        best_filter = filter_idx;
      }
    }
  }

  return best_filter;
}

void test_encode_adpcm(const std::vector<s16>& samples,
                       const std::vector<u8>& filter_debug,
                       const std::vector<u8>& shift_debug) {
  // the data is made of blocks.
  // Each block decodes to 28 samples.
  // each block has a shift and FIR filter.
  // the window is continuous across blocks.

  // we could try all combinations of filters / shifts and pick the best, but that's slow and
  // we don't know how to break ties if multiple are the same.
  // we will try all 5 filters, then be smart about picking the best shift from there.

  // filter coefficients.
  // there are 5x FIR filters that you can pick between.

  // last two samples from chosen encoding of the previous block
  // init to 0, like the decoder
  s32 prev_block_samples[2] = {0, 0};

  // TODO - this will drop some samples at the end, if we don't use a multiple of 28.
  // probably best to go back and pad with zeros or something.
  int block_count = samples.size() / SAMPLES_PER_BLOCK;

  for (int block_idx = 0; block_idx < block_count; block_idx++) {
    // try each filter
    s32 pre_shift_samples_per_filter[5][SAMPLES_PER_BLOCK];
    for (int filter_idx = 0; filter_idx < 5; filter_idx++) {
      encode_block_with_filter(filter_idx, samples.data() + SAMPLES_PER_BLOCK * block_idx,
                               pre_shift_samples_per_filter[filter_idx], prev_block_samples);
    }

    // this is somewhat arbitrary, but we will require that the largest delta in the previous encode
    // can be represented.

    s32 filter_errors[5] = {0, 0, 0, 0, 0};
    s32 filter_shifts[5] = {-1, -1, -1, -1};
    for (int filter_idx = 0; filter_idx < 5; filter_idx++) {
      // find the largest value
      s32 max_sample = INT32_MIN;
      s32 min_sample = INT32_MAX;

      bool debug = block_idx == 10966 && filter_idx == 4;

      for (int sample_idx = 0; sample_idx < SAMPLES_PER_BLOCK; sample_idx++) {
        s32 s = pre_shift_samples_per_filter[filter_idx][sample_idx];
        max_sample = std::max(s, max_sample);
        min_sample = std::min(s, min_sample);
      }

      if (debug) {
        lg::debug("Range: {}", max_sample - min_sample);
      }

      // see how many bits we need and pick shift.
      auto bits_for_max = std::max(4, std::max(get_max_bits(min_sample), get_max_bits(max_sample)));

      filter_shifts[filter_idx] = 4 + 12 - bits_for_max;

      filter_errors[filter_idx] = get_shift_error(filter_shifts[filter_idx],
                                                  pre_shift_samples_per_filter[filter_idx], debug);

      if (filter_errors[filter_idx] == 0) {
        while (filter_shifts[filter_idx] >= 0) {
          int next_error = get_shift_error(filter_shifts[filter_idx] - 1,
                                           pre_shift_samples_per_filter[filter_idx], false);
          if (next_error == 0) {
            filter_shifts[filter_idx]--;
          } else {
            break;
          }
        }
      }
    }

    int best_filter = break_filter_ties(filter_errors, filter_shifts);
    s32 best_shift = filter_shifts[best_filter];

    if (filter_errors[best_filter] || best_filter != filter_debug[block_idx] ||
        best_shift != shift_debug[block_idx]) {
      lg::error("Block {} me {}, {}  : answer {} {}: ERR {}", block_idx, best_filter, best_shift,
                filter_debug[block_idx], shift_debug[block_idx], filter_errors[best_filter]);
      lg::error("filter errors:");
      for (int i = 0; i < 5; i++) {
        lg::error(" [{}] {} {}", i, filter_errors[i], filter_shifts[i]);
      }
      ASSERT_MSG(false, fmt::format("prev: {} {}", prev_block_samples[0], prev_block_samples[1]));
    }

    prev_block_samples[0] = samples.at(block_idx * 28 + 27);
    prev_block_samples[1] = samples.at(block_idx * 28 + 26);

  }  // end loop over blocks
}

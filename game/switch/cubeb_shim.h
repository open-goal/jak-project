#pragma once

/*!
 * @file cubeb_shim.h
 * Minimal cubeb API over SDL2 audio, for the Switch build.
 *
 * cubeb is cross-compiled here with no backend at all (USE_ALSA/USE_PULSE/USE_WASAPI/... are all
 * empty in the Switch CMake cache), so cubeb_init() fails and 989snd never gets an output stream --
 * the port has simply been silent. devkitPro's SDL2 does have a working Switch audio backend
 * (audout), and 989snd only touches seven cubeb entry points with a single stereo S16LE 48kHz
 * stream, so wrapping SDL2 is far less work than writing a real cubeb backend.
 *
 * Everything is static inline on purpose: the definitions live only in the one translation unit
 * that uses them (989snd/player.cpp), so there is nothing for the still-linked but backend-less
 * libcubeb.a to collide with.
 */

#include <SDL2/SDL.h>

#include <cstdint>
#include <cstring>

#define CUBEB_OK 0
#define CUBEB_ERROR (-1)
#define CUBEB_STREAM_PREF_NONE 0

typedef struct cubeb cubeb;
typedef void* cubeb_devid;

enum cubeb_sample_format { CUBEB_SAMPLE_S16LE };
enum cubeb_channel_layout { CUBEB_LAYOUT_STEREO };
enum cubeb_state { CUBEB_STATE_STARTED, CUBEB_STATE_STOPPED, CUBEB_STATE_DRAINED, CUBEB_STATE_ERROR };

struct cubeb_stream_params {
  cubeb_sample_format format;
  uint32_t rate;
  uint32_t channels;
  cubeb_channel_layout layout;
  uint32_t prefs;
};

struct cubeb_stream;
typedef long (*cubeb_data_callback)(cubeb_stream* stream,
                                    void* user_ptr,
                                    const void* input_buffer,
                                    void* output_buffer,
                                    long nframes);
typedef void (*cubeb_state_callback)(cubeb_stream* stream, void* user_ptr, cubeb_state state);

struct cubeb_stream {
  SDL_AudioDeviceID device{0};
  cubeb_data_callback data_cb{nullptr};
  cubeb_state_callback state_cb{nullptr};
  void* user{nullptr};
  int bytes_per_frame{4};
};

static inline void sdl_shim_audio_callback(void* userdata, Uint8* stream, int len) {
  auto* s = static_cast<cubeb_stream*>(userdata);
  // 989snd mixes additively into whatever it is handed, so the buffer has to start silent.
  std::memset(stream, 0, static_cast<size_t>(len));
  if (s && s->data_cb) {
    s->data_cb(s, s->user, nullptr, stream, len / s->bytes_per_frame);
  }
}

static inline int cubeb_init(cubeb** context, const char* /*name*/, const char* /*backend*/) {
  if (SDL_InitSubSystem(SDL_INIT_AUDIO) != 0) {
    return CUBEB_ERROR;
  }
  // No per-context state is needed; a non-null sentinel keeps the caller's null checks meaningful.
  *context = reinterpret_cast<cubeb*>(1);
  return CUBEB_OK;
}

static inline int cubeb_get_min_latency(cubeb* /*context*/,
                                        cubeb_stream_params* /*params*/,
                                        uint32_t* latency_frames) {
  // SDL picks the real buffer size when the device opens; this is only a hint upstream.
  *latency_frames = 1024;
  return CUBEB_OK;
}

static inline int cubeb_stream_init(cubeb* /*context*/,
                                    cubeb_stream** stream,
                                    const char* /*stream_name*/,
                                    cubeb_devid /*input_device*/,
                                    cubeb_stream_params* /*input_params*/,
                                    cubeb_devid /*output_device*/,
                                    cubeb_stream_params* output_params,
                                    unsigned int latency_frames,
                                    cubeb_data_callback data_callback,
                                    cubeb_state_callback state_callback,
                                    void* user_ptr) {
  if (!output_params) {
    return CUBEB_ERROR;
  }
  auto* s = new cubeb_stream();
  s->data_cb = data_callback;
  s->state_cb = state_callback;
  s->user = user_ptr;
  s->bytes_per_frame = static_cast<int>(output_params->channels) * 2;  // S16 == 2 bytes/sample

  SDL_AudioSpec want{};
  want.freq = static_cast<int>(output_params->rate);
  want.format = AUDIO_S16LSB;
  want.channels = static_cast<Uint8>(output_params->channels);
  want.samples = static_cast<Uint16>(latency_frames ? latency_frames : 1024);
  want.callback = sdl_shim_audio_callback;
  want.userdata = s;

  SDL_AudioSpec have{};
  // Allow no conversions: 989snd generates exactly stereo S16LE at its own rate, and letting SDL
  // silently resample would desync the sequencer from the game's tick.
  s->device = SDL_OpenAudioDevice(nullptr, 0, &want, &have, 0);
  if (s->device == 0) {
    delete s;
    return CUBEB_ERROR;
  }
  s->bytes_per_frame = have.channels * 2;
  *stream = s;
  return CUBEB_OK;
}

static inline int cubeb_stream_start(cubeb_stream* stream) {
  if (!stream || stream->device == 0) {
    return CUBEB_ERROR;
  }
  SDL_PauseAudioDevice(stream->device, 0);
  if (stream->state_cb) {
    stream->state_cb(stream, stream->user, CUBEB_STATE_STARTED);
  }
  return CUBEB_OK;
}

static inline int cubeb_stream_stop(cubeb_stream* stream) {
  if (!stream || stream->device == 0) {
    return CUBEB_ERROR;
  }
  SDL_PauseAudioDevice(stream->device, 1);
  if (stream->state_cb) {
    stream->state_cb(stream, stream->user, CUBEB_STATE_STOPPED);
  }
  return CUBEB_OK;
}

static inline void cubeb_stream_destroy(cubeb_stream* stream) {
  if (!stream) {
    return;
  }
  if (stream->device != 0) {
    SDL_CloseAudioDevice(stream->device);
  }
  delete stream;
}

static inline void cubeb_destroy(cubeb* /*context*/) {
  SDL_QuitSubSystem(SDL_INIT_AUDIO);
}

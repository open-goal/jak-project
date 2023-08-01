set(CMAKE_CXX_STANDARD 17)

set(SOUND_SOURCES
  989snd/player.cpp
  989snd/midi_handler.cpp
  989snd/ame_handler.cpp
  989snd/blocksound_handler.cpp
  989snd/musicbank.cpp
  989snd/sfxblock.cpp
  989snd/sfxblock2.cpp
  989snd/sfxgrain.cpp
  989snd/loader.cpp
  989snd/vagvoice.cpp
  989snd/lfo.cpp
  989snd/util.cpp
  common/synth.cpp
  common/voice.cpp
  common/envelope.cpp
  sndshim.cpp
  sdshim.cpp
  )

add_library(sound STATIC ${SOUND_SOURCES})
target_link_libraries(sound PRIVATE fmt cubeb common)

add_executable(sndplay 989snd/sndplay.cpp)
if(WIN32)
    target_link_libraries(sndplay PRIVATE sound cubeb)
elseif(APPLE)
    target_link_libraries(sndplay PRIVATE sound cubeb)
else()
    target_link_libraries(sndplay PRIVATE sound cubeb stdc++fs)
endif()

if (NOT WIN32)
    target_compile_options(sound
            PRIVATE
            -Wno-unknown-warning-option
            -Wno-unused-private-field
            -Wno-unused-parameter
            -Wno-shadow
            )
endif()

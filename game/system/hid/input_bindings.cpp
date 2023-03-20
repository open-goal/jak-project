#include "input_bindings.h"

#include "third-party/SDL/include/SDL.h"

const std::vector<PadData::ButtonIndex> PAD_DATA_PRESSURE_INDEX_ORDER = {
    PadData::ButtonIndex::DPAD_RIGHT, PadData::ButtonIndex::DPAD_LEFT,
    PadData::ButtonIndex::DPAD_UP,    PadData::ButtonIndex::DPAD_DOWN,
    PadData::ButtonIndex::TRIANGLE,   PadData::ButtonIndex::CIRCLE,
    PadData::ButtonIndex::CROSS,      PadData::ButtonIndex::SQUARE,
    PadData::ButtonIndex::L1,         PadData::ButtonIndex::R1,
    PadData::ButtonIndex::L2,         PadData::ButtonIndex::R2};

// TODO - make sure a u8 is good enough, might not be!
const InputBindingGroups DEFAULT_CONTROLLER_BINDS = {
    {{SDL_CONTROLLER_AXIS_LEFTX, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDL_CONTROLLER_AXIS_LEFTY, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDL_CONTROLLER_AXIS_RIGHTX, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDL_CONTROLLER_AXIS_RIGHTY, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}}},
    {
        {SDL_CONTROLLER_AXIS_TRIGGERLEFT, {InputBinding(PadData::ButtonIndex::L2)}},
        {SDL_CONTROLLER_AXIS_TRIGGERRIGHT, {InputBinding(PadData::ButtonIndex::R2)}},
    },
    {{SDL_CONTROLLER_BUTTON_A, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDL_CONTROLLER_BUTTON_B, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDL_CONTROLLER_BUTTON_X, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDL_CONTROLLER_BUTTON_Y, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDL_CONTROLLER_BUTTON_LEFTSTICK, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSTICK, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDL_CONTROLLER_BUTTON_BACK, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDL_CONTROLLER_BUTTON_START, {InputBinding(PadData::ButtonIndex::START)}},
     {SDL_CONTROLLER_BUTTON_LEFTSHOULDER, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSHOULDER, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDL_CONTROLLER_BUTTON_DPAD_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDL_CONTROLLER_BUTTON_DPAD_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDL_CONTROLLER_BUTTON_DPAD_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDL_CONTROLLER_BUTTON_DPAD_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}}};

const InputBindingGroups DEFAULT_KEYBOARD_BINDS = {
    {{SDLK_a, {InputBinding(PadData::AnalogIndex::LEFT_X, true)}},
     {SDLK_d, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDLK_s, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDLK_w, {InputBinding(PadData::AnalogIndex::LEFT_Y, true)}},
     {SDLK_l, {InputBinding(PadData::AnalogIndex::RIGHT_X, true)}},
     {SDLK_j, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDLK_k, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}},
     {SDLK_i, {InputBinding(PadData::AnalogIndex::RIGHT_Y, true)}}},
    {},
    {{SDLK_SPACE, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDLK_f, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDLK_e, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDLK_r, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDLK_COMMA, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDLK_PERIOD, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDLK_QUOTE, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDLK_RETURN, {InputBinding(PadData::ButtonIndex::START)}},
     {SDLK_o, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDLK_q, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDLK_1, {InputBinding(PadData::ButtonIndex::L2)}},
     {SDLK_p, {InputBinding(PadData::ButtonIndex::R2)}},
     {SDLK_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDLK_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDLK_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDLK_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}}};

const InputBindingGroups DEFAULT_MOUSE_BINDS = {{}, {}, {}};

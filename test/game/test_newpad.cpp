
#include <algorithm>
#include <cmath>

#include "game/graphics/pipelines/opengl.h"
#include "game/system/newpad.h"
#include "gtest/gtest.h"
//#include "gmock/gmock.h"

class PeripheralTest : public ::testing::Test {
 public:
  PeripheralTest() {
    Pad::ForceClearKeys();
    Pad::ForceClearAnalogValue();

    for (int i = 0; i < Pad::CONTROLLER_COUNT; ++i) {
      Pad::clear_pad(i);
      Pad::SetGamepadState(i, -1);
    }
    Pad::DefaultMapping(mapping_info_);
  };
  ~PeripheralTest(){};

 protected:
  Pad::MappingInfo mapping_info_;
};

TEST_F(PeripheralTest, UpdatePad_KeyboardPad_ClearsControllerInputBuffers) {
  // Arrange
  bool expected_controller_status = false;

  bool* controller_input_buffer = Pad::GetControllerInputBuffer(0);
  ::memset(controller_input_buffer, true,
           sizeof(controller_input_buffer[0]) * (int)Pad::Button::Max);

  // Act
  Pad::update_gamepads(mapping_info_);

  // Assert
  for (int i = 0; i < (int)Pad::Button::Max; ++i) {
    EXPECT_EQ(expected_controller_status, controller_input_buffer[i]);
  }
}
TEST_F(PeripheralTest, ClearKey_ValidKey_UpdateKeyboardBuffer) {
  // Arrange
  int input_key = 127;
  bool expected_buffer_key_status = false;

  bool* actual_keyboard_buffer = Pad::GetKeyboardBufferedInputBuffer();
  ::memset(actual_keyboard_buffer, true, sizeof(*actual_keyboard_buffer) * glfw::NUM_KEYS);

  // Act
  Pad::ClearKey(input_key);

  // Assert
  bool actual_buffer_key_status = actual_keyboard_buffer[input_key];
  EXPECT_EQ(expected_buffer_key_status, actual_buffer_key_status);
}
TEST_F(PeripheralTest, ClearKey_InvalidKey_DoNothing) {
  // Arrange
  int input_key = glfw::NUM_KEYS + 1;
  bool expected_buffer_key_status = true;

  bool* actual_keyboard_buffer = Pad::GetKeyboardBufferedInputBuffer();
  ::memset(actual_keyboard_buffer, true, sizeof(*actual_keyboard_buffer) * glfw::NUM_KEYS);

  // Act
  Pad::ClearKey(input_key);

  // Assert
  for (int i = 0; i < glfw::NUM_KEYS; ++i) {
    EXPECT_EQ(expected_buffer_key_status, actual_keyboard_buffer[i]);
  }
}
TEST_F(PeripheralTest, SetAnalogAxisValue_NominalAnalogAxisX_SetConvertedValue) {
  // Arrange
  float expected_analog_value = 0;
  std::swap(mapping_info_.controller_analog_mapping[0][(int)Pad::Analog::Left_X],
            mapping_info_.controller_analog_mapping[0][(int)Pad::Analog::Right_X]);

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS), 100.0);

  // Assert
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(0);
  EXPECT_FLOAT_EQ(expected_analog_value, keyboard_analog_buffer[(int)Pad::Analog::Left_X]);
}
TEST_F(PeripheralTest, SetAnalogAxisValue_NominalAnalogAxisY_SetConvertedValue) {  // Arrange
  float expected_analog_value = 0;
  std::swap(mapping_info_.controller_analog_mapping[0][(int)Pad::Analog::Left_Y],
            mapping_info_.controller_analog_mapping[0][(int)Pad::Analog::Right_Y]);

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS), 100.0);

  // Assert
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(0);
  EXPECT_FLOAT_EQ(expected_analog_value, keyboard_analog_buffer[(int)Pad::Analog::Left_Y]);
}
TEST_F(PeripheralTest, SetAnalogAxisValue_InputLargerThanMaxValue_SetMaxValue) {
  // Arrange
  float expected_analog_value = 1;

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS), 100.0);

  // Assert
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(0);
  EXPECT_FLOAT_EQ(expected_analog_value, keyboard_analog_buffer[(int)Pad::Analog::Right_X]);
}
TEST_F(PeripheralTest, SetAnalogAxisValue_InputSmallerThanMinValue_SetMinValue) {
  // Arrange
  float expected_analog_value = -1;

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS),
                          -100.0);

  // Assert
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(0);
  EXPECT_FLOAT_EQ(expected_analog_value, keyboard_analog_buffer[(int)Pad::Analog::Right_X]);
}
TEST_F(PeripheralTest, SetAnalogAxisValue_InputIsNAN_SetZero) {
  // Arrange
  float expected_analog_value = 0;

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS),
                          std::nan("1"));

  // Assert
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(0);
  EXPECT_FLOAT_EQ(expected_analog_value, keyboard_analog_buffer[(int)Pad::Analog::Right_X]);
}
TEST_F(
    PeripheralTest,
    SetAnalogAxisValue_MouseXAxisSensitivityLowerThanMinimumSensitivty_SetMouseSensitivityToMinimum) {
  // Arrange
  float expected_x_axis_sensitivity = 1e-4;
  mapping_info_.mouse_x_axis_sensitivities[0] = 0;

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_X_AXIS), 100);

  // Assert
  EXPECT_FLOAT_EQ(expected_x_axis_sensitivity, mapping_info_.mouse_x_axis_sensitivities[0]);
}
TEST_F(
    PeripheralTest,
    SetAnalogAxisValue_MouseYAxisSensitivityLowerThanMinimumSensitivty_SetMouseSensitivityToMinimum) {
  // Arrange
  float expected_y_axis_sensitivity = 1e-4;
  mapping_info_.mouse_y_axis_sensitivities[0] = 0;

  // Act
  Pad::SetAnalogAxisValue(mapping_info_, static_cast<int>(GlfwKeyCustomAxis::CURSOR_Y_AXIS), 100);

  // Assert
  EXPECT_FLOAT_EQ(expected_y_axis_sensitivity, mapping_info_.mouse_y_axis_sensitivities[0]);
}

TEST_F(PeripheralTest, UpdateAxisValue_XAxisPositiveKey_IncrementValue) {
  // Arrange
  float expected_analog_value = 1.0f;
  int pad_index = 0;
  int key = GLFW_KEY_D;
  bool* keyboard_buffered_key_buffer = Pad::GetKeyboardBufferedInputBuffer();
  keyboard_buffered_key_buffer[key] = true;

  // Act
  Pad::UpdateAxisValue(mapping_info_);

  // Arrange
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(pad_index);
  float actual_analog_value = keyboard_analog_buffer[(int)Pad::Analog::Left_X];
  EXPECT_FLOAT_EQ(expected_analog_value, actual_analog_value);
}
TEST_F(PeripheralTest, UpdateAxisValue_YAxisPositiveKey_DecrementValue) {
  // Arrange
  float expected_analog_value = -1.0f;
  int pad_index = 0;
  int key = GLFW_KEY_W;
  bool* keyboard_buffered_key_buffer = Pad::GetKeyboardBufferedInputBuffer();
  keyboard_buffered_key_buffer[key] = true;

  // Act
  Pad::UpdateAxisValue(mapping_info_);

  // Arrange
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(pad_index);
  float actual_analog_value = keyboard_analog_buffer[(int)Pad::Analog::Left_Y];
  EXPECT_FLOAT_EQ(expected_analog_value, actual_analog_value);
}
TEST_F(PeripheralTest, UpdateAxisValue_XAxisNegativeKey_DecrementValue) {
  // Arrange
  int pad_index = 0;
  float expected_analog_value = -1.0f;

  int key = GLFW_KEY_A;
  bool* keyboard_buffered_key_buffer = Pad::GetKeyboardBufferedInputBuffer();
  keyboard_buffered_key_buffer[key] = true;

  // Act
  Pad::UpdateAxisValue(mapping_info_);

  // Arrange
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(pad_index);
  float actual_analog_value = keyboard_analog_buffer[(int)Pad::Analog::Left_X];
  EXPECT_FLOAT_EQ(expected_analog_value, actual_analog_value);
}

TEST_F(PeripheralTest, UpdateAxisValue_RightYAxisPositiveKey_IncrementValue) {
  // Arrange
  float expected_analog_value = 1.0f;
  int pad_index = 0;
  int key = GLFW_KEY_S;
  bool* keyboard_buffered_key_buffer = Pad::GetKeyboardBufferedInputBuffer();
  keyboard_buffered_key_buffer[key] = true;

  // Act
  Pad::UpdateAxisValue(mapping_info_);

  // Arrange
  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(pad_index);
  float actual_analog_value = keyboard_analog_buffer[(int)Pad::Analog::Left_Y];
  EXPECT_FLOAT_EQ(expected_analog_value, actual_analog_value);
}

TEST_F(PeripheralTest, GetAnalogValue_InvalidPadId_ReturnsNeutralPosition) {
  // Arrange
  int expected_analog_status = 127;

  // Act
  int actual_analog_status = Pad::GetAnalogValue(mapping_info_, Pad::Analog::Left_X, 2);

  // Assert
  EXPECT_EQ(expected_analog_status, actual_analog_status);
}
TEST_F(PeripheralTest, GetAnalogValue_ControllerPad_ReturnsAnalogValue) {
  // Arrange
  int expected_analog_status = 0;

  int pad_index = 0;
  int controller_index = 0;
  Pad::SetGamepadState(pad_index, controller_index);

  float* controller_analog_buffer = Pad::GetControllerAnalogInputBuffer(pad_index);
  controller_analog_buffer[(int)Pad::Analog::Left_X] = -1.0f;

  // Act
  int actual_analog_status = Pad::GetAnalogValue(mapping_info_, Pad::Analog::Left_X, 0);

  // Assert
  EXPECT_EQ(expected_analog_status, actual_analog_status);
}
TEST_F(PeripheralTest, GetAnalogValue_KeyboardPad_ReturnsAnalogValue) {
  // Arrange
  int expected_analog_status = 255;
  int pad_index = 0;

  float* keyboard_analog_buffer = Pad::GetKeyboardInputAnalogBuffer(pad_index);
  keyboard_analog_buffer[(int)Pad::Analog::Left_X] = 1.0f;

  // Act
  int actual_analog_status = Pad::GetAnalogValue(mapping_info_, Pad::Analog::Left_X, 0);

  // Assert
  EXPECT_EQ(expected_analog_status, actual_analog_status);
}

TEST_F(PeripheralTest, IsPressed_InvalidPadId_ReturnsFalse) {
  // Arrange
  int expected_button_status = 0;

  // Act
  int actual_button_status = Pad::IsPressed(mapping_info_, Pad::Button::X, 2);

  // Assert
  EXPECT_EQ(expected_button_status, actual_button_status);
}
TEST_F(PeripheralTest, IsPressed_ControllerPad_ReturnsControllerBufferValue) {
  // Arrange
  int expected_button_status = 1;

  int pad_index = 0;
  int controller_index = 0;
  Pad::SetGamepadState(pad_index, controller_index);

  bool* controller_button_status_buffer = Pad::GetControllerInputBuffer(pad_index);
  controller_button_status_buffer[(int)Pad::Button::X] = expected_button_status;

  // Act
  int actual_button_status = Pad::IsPressed(mapping_info_, Pad::Button::X, pad_index);

  // Assert
  EXPECT_EQ(expected_button_status, actual_button_status);
}
TEST_F(PeripheralTest, IsPressed_KeyboardPad_ReturnsKeyboardBufferValue) {
  // Arrange
  int expected_button_status = 1;
  bool* keyboard_buffered_key_status_buffer = Pad::GetKeyboardBufferedInputBuffer();
  keyboard_buffered_key_status_buffer[GLFW_KEY_SPACE] = expected_button_status;

  // Act
  int actual_button_status = Pad::IsPressed(mapping_info_, Pad::Button::X, 0);

  // Assert
  EXPECT_EQ(expected_button_status, actual_button_status);
}

// TODO: InputModeStatus Tests

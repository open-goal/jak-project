#pragma once

namespace jak2 {
void InitParms(int argc, const char* const* argv);
void InitIOP();
int InitMachine();
int ShutdownMachine();
void InitMachineScheme();

struct MouseInfo {};
}  // namespace jak2
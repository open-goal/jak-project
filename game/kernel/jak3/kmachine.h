#pragma once

namespace jak3 {
void InitParms(int argc, const char* const* argv);
void InitMachineScheme();
int InitMachine();
int ShutdownMachine();
}  // namespace jak3
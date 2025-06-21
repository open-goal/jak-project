#pragma once

namespace jakx {
void InitParms(int argc, const char* const* argv);
void InitMachineScheme();
int InitMachine();
int ShutdownMachine();
}  // namespace jakx
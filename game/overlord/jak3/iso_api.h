#pragma once

namespace jak3 {
void jak3_overlord_init_globals_iso_api();

struct ISOFileDef;

void LoadISOFileToEE(const ISOFileDef* file_def, int addr, int length);
int LoadISOFileToIOP(const ISOFileDef* file_def, void* addr, int length);

}
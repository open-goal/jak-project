#pragma once

#ifndef JAK_V2_OVERLORD_H
#define JAK_V2_OVERLORD_H

int start_overlord(int argc, const char* const* argv);
int start_overlord_wrapper(int argc, const char* const* argv, bool* signal);
void ExitIOP();

#endif  // JAK_V2_OVERLORD_H

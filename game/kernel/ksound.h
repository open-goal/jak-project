#pragma once

/*!
 * @file ksound.h
 * There's not much here. My guess is this was set up as framework to match the kmachine.cpp format,
 * but whoever did the sound didn't use this.
 */

#ifndef JAK_KSOUND_H
#define JAK_KSOUND_H

void InitSound();
void ShutdownSound();
void InitSoundScheme();

#endif  // JAK_KSOUND_H

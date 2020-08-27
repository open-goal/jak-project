/*!
 * @file Coloring.h
 * High Level Interface for register coloring.
 */

#ifndef JAK_COLORING_H
#define JAK_COLORING_H

#include <memory>

class FunctionEnv;
bool do_linear_scan_coloring(FunctionEnv& func);

#endif  // JAK_COLORING_H

/*!
 * @file BasicOpBuilder.h
 * Analyzes a basic block and converts instructions to BasicOps.
 * These will be used later to convert the Cfg into the nested IR format.
 */

#pragma once

class Function;
struct BasicBlock;
class LinkedObjectFile;

void add_basic_ops_to_block(Function* func, const BasicBlock& block, LinkedObjectFile* file);
#pragma once

namespace decompiler {

class LabelDB;
class LinkedObjectFile;
class DecompilerTypeSystem;

void analyze_labels(LabelDB* db, LinkedObjectFile* file, const DecompilerTypeSystem& dts);
}  // namespace decompiler
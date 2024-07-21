#include "iso_fake.h"

namespace jak3 {
CFakeISOCDFileSystem g_FakeISOCDFileSystem;

int CFakeISOCDFileSystem::Init() {
  return 0;
}

void CFakeISOCDFileSystem::PollDrive() {}

const ISOFileDef* CFakeISOCDFileSystem::Find(const char* name) {
  return nullptr;
}

const ISOFileDef* CFakeISOCDFileSystem::FindIN(const char* name) {
  return nullptr;
}

int CFakeISOCDFileSystem::GetLength(const ISOFileDef* def) {
  return 0;
}

CBaseFile* CFakeISOCDFileSystem::Open(const ISOFileDef* def, int offset, EFileComp mode) {
  return nullptr;
}

CBaseFile* CFakeISOCDFileSystem::OpenWad(const ISOFileDef* def, int offset) {
  return nullptr;
}

VagDirEntryJak3* CFakeISOCDFileSystem::FindVagFile(const char* name) {
  return nullptr;
}

}  // namespace jak3

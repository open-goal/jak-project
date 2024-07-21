#pragma once

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/iso_structs.h"

namespace jak3 {
class CBaseFileSystem {
 public:
  virtual int Init() = 0;
  virtual void PollDrive() = 0;
  virtual const ISOFileDef* Find(const char* name) = 0;
  virtual const ISOFileDef* FindIN(const char* name) = 0;
  virtual int GetLength(const ISOFileDef* def) = 0;
  virtual CBaseFile* Open(const ISOFileDef* def, int offset, EFileComp mode) = 0;
  virtual CBaseFile* OpenWad(const ISOFileDef* def, int offset) = 0;
  virtual VagDirEntryJak3* FindVagFile(const char* name) = 0;

  u8* CheckForPageBoundaryCrossing();

 private:
    ISOBuffer m_Buffer;
};

}  // namespace jak3

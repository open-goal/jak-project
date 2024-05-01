#ifndef ISO_FAKE_H_
#define ISO_FAKE_H_

#include "basefile.h"
#include "basefilesystem.h"

#include "game/overlord/jak3/iso.h"

namespace jak3 {
class CFakeISOCDFileSystem : public CBaseFileSystem {
  int Init() override;
  void PollDrive() override;
  const ISOFileDef* Find(const char* name) override;
  const ISOFileDef* FindIN(const char* name) override;
  int GetLength(const ISOFileDef* def) override;
  int Open(const ISOFileDef* def, int offset, EFileComp mode) override;
  int OpenWad(const ISOFileDef* def, int offset) override;
  VagDirEntryJak3* FindVagFile(const char* name) override;
};

class CFakeISOCDFile : public CBaseFile {
  EIsoStatus BeginRead(ISOBuffer*) override;
  EIsoStatus SyncRead() override;
  void Close() override;

  /* unk return values */
  void Unk1() override;
  void Unk2() override;
};

extern CFakeISOCDFileSystem g_FakeISOCDFileSystem;

}  // namespace jak3

#endif  // ISO_FAKE_H_

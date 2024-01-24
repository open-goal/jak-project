#ifndef BASEFILE_H_
#define BASEFILE_H_

#include "game/overlord/jak3/iso_structs.h"
#include "game/overlord/jak3/overlord.h"

namespace jak3 {
class CBaseFile {
 public:
  CBaseFile(const ISOFileDef*);
  virtual EIsoStatus BeginRead(ISOBuffer*) = 0;
  virtual EIsoStatus SyncRead() = 0;
  virtual void Close() = 0;

    /* unk return values */
  virtual void Unk1() = 0;
  virtual void Unk2() = 0;
};
}  // namespace jak3

#endif  // BASEFILE_H_

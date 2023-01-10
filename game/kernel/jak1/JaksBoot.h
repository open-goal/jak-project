#include "game/IBoot.h"

class JaksBoot : public IBoot {
 public:
  virtual ~JaksBoot() = default;

  virtual s32 goal_main(int argc, const char* const* argv) override;
  virtual void init_globals(void) override;
};

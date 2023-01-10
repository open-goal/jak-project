#pragma once

#include "game/IBoot.h"
#include "game/IGfx.h"

class Game {
 private:
  Game() {}
  Game(const Game& obj) = delete;
  Game& operator=(const Game&) = delete;
  IBoot* m_Boot = nullptr;
  IGfx* m_Gfx = nullptr;

 public:
  static Game* getInstance() {
    static Game* instance;
    if (instance == nullptr) {
      instance = new Game();
      //            instance->Init();
    }
    return instance;
  }
  u32 Init();
  u32 Shutdown();
  IBoot& Boot() const { return *m_Boot; }
  IGfx& Gfx() const { return *m_Gfx; }
};

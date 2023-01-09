#include "game/Game.h"

#ifndef CUSTOM_OPENGOAL

#include "game/kernel/jak1/JaksBoot.h"
#include "game/graphics/JaksGfx.h"

u32 Game::Init(){
    m_Boot = new JaksBoot();
    m_Gfx = new JaksGfx();

    return 1;
}
u32 Game::Shutdown(){
    if(m_Boot != nullptr){
        delete m_Boot;
    }
    if(m_Gfx != nullptr){
        delete m_Gfx;
    }

    return 1;
}
#endif // CUSTOM_OPENGOAL

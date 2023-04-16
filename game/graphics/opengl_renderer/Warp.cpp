#include "Warp.h"

Warp::Warp(const std::string& name, int id)
    : BucketRenderer(name, id), m_generic(name, id) {}

Warp::~Warp() {}

void Warp::draw_debug_window() {
  m_generic.draw_debug_window();
}

void Warp::render(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  m_generic.render_in_mode(dma, render_state, prof, Generic2::Mode::WARP);
}

void Warp::init_shaders(ShaderLibrary& shaders) {
  m_generic.init_shaders(shaders);
}
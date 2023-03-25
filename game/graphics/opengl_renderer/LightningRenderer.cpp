#include "LightningRenderer.h"

LightningRenderer::LightningRenderer(const std::string& name, int id)
    : BucketRenderer(name, id), m_generic(name, id) {}

LightningRenderer::~LightningRenderer() {}

void LightningRenderer::draw_debug_window() {
  m_generic.draw_debug_window();
}

void LightningRenderer::render(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  m_generic.render_in_mode(dma, render_state, prof, Generic2::Mode::LIGHTNING);
}

void LightningRenderer::init_shaders(ShaderLibrary& shaders) {
  m_generic.init_shaders(shaders);
}
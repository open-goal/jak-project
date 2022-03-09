#include "mips2c_table.h"
#include "common/log/log.h"

#include "game/kernel/kmalloc.h"
#include "game/kernel/kscheme.h"
#include "common/symbols.h"

extern "C" {
void _mips2c_call_linux();
void _mips2c_call_windows();
}

namespace Mips2C {

namespace draw_string {
extern void link();
}

namespace sp_init_fields {
extern void link();
}

namespace particle_adgif {
extern void link();
}

namespace sp_launch_particles_var {
extern void link();
}

namespace sp_process_block_3d {
extern void link();
}

namespace sp_process_block_2d {
extern void link();
}

namespace draw_large_polygon {
extern void link();
}

namespace init_sky_regs {
extern void link();
}

namespace clip_polygon_against_positive_hyperplane {
extern void link();
}

namespace render_sky_quad {
extern void link();
}

namespace render_sky_tri {
extern void link();
}

namespace set_tex_offset {
extern void link();
}

namespace set_sky_vf27 {
extern void link();
}

namespace set_sky_vf23_value {
extern void link();
}

namespace adgif_shader_texture_with_update {
extern void link();
}

namespace init_boundary_regs {
extern void link();
}

namespace render_boundary_quad {
extern void link();
}

namespace render_boundary_tri {
extern void link();
}

namespace draw_boundary_polygon {
extern void link();
}

namespace draw_inline_array_tfrag {
extern void link();
}

namespace stats_tfrag_asm {
extern void link();
}

namespace time_of_day_interp_colors_scratch {
extern void link();
}

namespace collide_do_primitives {
extern void link();
}

namespace moving_sphere_triangle_intersect {
extern void link();
}

namespace method_12_collide_mesh {
extern void link();
}

namespace method_11_collide_mesh {
extern void link();
}

namespace collide_probe_node {
extern void link();
}

namespace collide_probe_instance_tie {
extern void link();
}

namespace method_26_collide_cache {
extern void link();
}

namespace method_32_collide_cache {
extern void link();
}

namespace pc_upload_collide_frag {
extern void link();
}

namespace method_28_collide_cache {
extern void link();
}

namespace method_27_collide_cache {
extern void link();
}

namespace method_29_collide_cache {
extern void link();
}

namespace method_12_collide_shape_prim_mesh {
extern void link();
}

namespace method_14_collide_shape_prim_mesh {
extern void link();
}

namespace method_13_collide_shape_prim_mesh {
extern void link();
}

namespace method_30_collide_cache {
extern void link();
}

namespace method_9_collide_cache_prim {
extern void link();
}

namespace method_10_collide_cache_prim {
extern void link();
}

namespace method_10_collide_puss_work {
extern void link();
}

namespace method_9_collide_puss_work {
extern void link();
}
namespace method_15_collide_mesh {
extern void link();
}

namespace method_14_collide_mesh {
extern void link();
}

namespace method_16_collide_edge_work {
extern void link();
}

namespace method_15_collide_edge_work {
extern void link();
}

namespace method_10_collide_edge_hold_list {
extern void link();
}

namespace method_18_collide_edge_work {
extern void link();
}

namespace calc_animation_from_spr {
extern void link();
}
namespace bones_mtx_calc {
extern void link();
}
namespace cspace_parented_transformq_joint {
extern void link();
}
namespace draw_bones_merc {
extern void link();
}
namespace draw_bones_check_longest_edge_asm {
extern void link();
}
namespace blerc_execute {
extern void link();
}
namespace setup_blerc_chains_for_one_fragment {
extern void link();
}
namespace generic_merc_init_asm {
extern void link();
}
namespace generic_merc_execute_asm {
extern void link();
}
namespace mercneric_convert {
extern void link();
}
namespace generic_prepare_dma_double {
extern void link();
}
namespace generic_light_proc {
extern void link();
}
namespace generic_envmap_proc {
extern void link();
}
namespace high_speed_reject {
extern void link();
}
namespace generic_prepare_dma_single {
extern void link();
}
namespace ripple_create_wave_table {
extern void link();
}
namespace ripple_execute_init {
extern void link();
}
namespace ripple_apply_wave_table {
extern void link();
}
namespace ripple_matrix_scale {
extern void link();
}
namespace init_ocean_far_regs {
extern void link();
}
namespace render_ocean_quad {
extern void link();
}
namespace draw_large_polygon_ocean {
extern void link();
}
namespace ocean_interp_wave {
extern void link();
}
namespace ocean_generate_verts {
extern void link();
}
LinkedFunctionTable gLinkedFunctionTable;
Rng gRng;
std::unordered_map<std::string, std::vector<void (*)()>> gMips2CLinkCallbacks = {
    {"font", {draw_string::link}},
    {"sparticle-launcher",
     {sp_init_fields::link, particle_adgif::link, sp_launch_particles_var::link}},
    {"sparticle", {sp_process_block_3d::link, sp_process_block_2d::link}},
    {"texture", {adgif_shader_texture_with_update::link}},
    {"sky-tng",
     {draw_large_polygon::link, init_sky_regs::link, clip_polygon_against_positive_hyperplane::link,
      render_sky_quad::link, render_sky_tri::link, set_tex_offset::link, set_sky_vf27::link,
      set_sky_vf23_value::link}},
    {"load-boundary",
     {init_boundary_regs::link, render_boundary_quad::link, render_boundary_tri::link,
      draw_boundary_polygon::link}},
    {"tfrag", {draw_inline_array_tfrag::link, stats_tfrag_asm::link}},
    {"time-of-day", {time_of_day_interp_colors_scratch::link}},
    {"collide-func", {collide_do_primitives::link, moving_sphere_triangle_intersect::link}},
    {"collide-probe", {collide_probe_node::link, collide_probe_instance_tie::link}},
    {"collide-mesh",
     {method_12_collide_mesh::link, method_11_collide_mesh::link, method_15_collide_mesh::link,
      method_14_collide_mesh::link}},
    {"collide-cache",
     {method_26_collide_cache::link, method_32_collide_cache::link, pc_upload_collide_frag::link,
      method_28_collide_cache::link, method_27_collide_cache::link, method_29_collide_cache::link,
      method_12_collide_shape_prim_mesh::link, method_14_collide_shape_prim_mesh::link,
      method_13_collide_shape_prim_mesh::link, method_30_collide_cache::link,
      method_9_collide_cache_prim::link, method_10_collide_cache_prim::link,
      method_10_collide_puss_work::link, method_9_collide_puss_work::link}},
    {"collide-edge-grab",
     {method_16_collide_edge_work::link, method_15_collide_edge_work::link,
      method_10_collide_edge_hold_list::link, method_18_collide_edge_work::link}},
    {"joint", {calc_animation_from_spr::link, cspace_parented_transformq_joint::link}},
    {"bones",
     {bones_mtx_calc::link, draw_bones_merc::link, draw_bones_check_longest_edge_asm::link}},
    {"merc-blend-shape", {blerc_execute::link, setup_blerc_chains_for_one_fragment::link}},
    {"generic-merc",
     {generic_merc_init_asm::link, generic_merc_execute_asm::link, mercneric_convert::link,
      high_speed_reject::link}},
    {"generic-effect",
     {generic_prepare_dma_double::link, generic_light_proc::link, generic_envmap_proc::link,
      generic_prepare_dma_single::link}},
    {"ripple",
     {ripple_execute_init::link, ripple_create_wave_table::link, ripple_apply_wave_table::link,
      ripple_matrix_scale::link}},
    {"ocean", {init_ocean_far_regs::link, render_ocean_quad::link, draw_large_polygon_ocean::link}},
    {"ocean-vu0", {ocean_interp_wave::link, ocean_generate_verts::link}}};

void LinkedFunctionTable::reg(const std::string& name, u64 (*exec)(void*), u32 stack_size) {
  const auto& it = m_executes.insert({name, {exec, Ptr<u8>()}});
  if (!it.second) {
    lg::error("MIPS2C Function {} is registered multiple times, ignoring later registrations.",
              name);
  }

  // this is short stub that will jump to the appropriate function.
  auto jump_to_asm = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                               *(s7 + FIX_SYM_FUNCTION_TYPE), 0x40, UNKNOWN_PP));
  it.first->second.goal_trampoline = jump_to_asm;

  u8* ptr = jump_to_asm.c();

  {
    // linux

    // push the function
    u64 addr = (u64)exec;
    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;
    *ptr = 0x50;
    ptr++;

    // push the stack size
    addr = stack_size;
    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;
    *ptr = 0x50;
    ptr++;

    // call the other function
#ifdef __linux__
    addr = (u64)_mips2c_call_linux;
#elif _WIN32
    addr = (u64)_mips2c_call_windows;
#endif

    *ptr = 0x48;
    ptr++;
    *ptr = 0xb8;
    ptr++;
    memcpy(ptr, &addr, 8);
    ptr += 8;

    // jumps to the mips2c call, which will return to the caller of this stub.
    *ptr = 0xff;
    ptr++;
    *ptr = 0xe0;
  }
}

u32 LinkedFunctionTable::get(const std::string& name) {
  auto it = m_executes.find(name);
  if (it == m_executes.end()) {
    ASSERT(false);
  }
  return it->second.goal_trampoline.offset;
}
}  // namespace Mips2C
# We define our own compilation flags here.
set(CMAKE_CXX_STANDARD 17)

enable_language(ASM_NASM)
set(CMAKE_ASM_NASM_SOURCE_FILE_EXTENSIONS ${CMAKE_ASM_NASM_SOURCE_FILE_EXTENSIONS} asm)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> -f ${CMAKE_ASM_NASM_OBJECT_FORMAT} -o <OBJECT> <SOURCE>")
set_source_files_properties(kernel/asm_funcs.asm PROPERTIES COMPILE_FLAGS "-g")
set(RUNTIME_SOURCE
        runtime.cpp
        discord.cpp
        system/SystemThread.cpp
        system/IOP_Kernel.cpp
        system/iop_thread.cpp
        system/Deci2Server.cpp
        system/newpad.cpp
        sce/libcdvd_ee.cpp
        sce/libscf.cpp
        sce/libdma.cpp
        sce/libpad.cpp
        sce/libgraph.cpp
        sce/deci2.cpp
        sce/sif_ee.cpp
        sce/sif_ee_memcard.cpp
        sce/iop.cpp
        sce/stubs.cpp
        kernel/asm_funcs.asm
        kernel/fileio.cpp
        kernel/kboot.cpp
        kernel/kdgo.cpp
        kernel/kdsnetm.cpp
        kernel/klink.cpp
        kernel/klisten.cpp
        kernel/kmachine.cpp
        kernel/kmalloc.cpp
        kernel/kmemcard.cpp
        kernel/kprint.cpp
        kernel/kscheme.cpp
        kernel/ksocket.cpp
        kernel/ksound.cpp
        mips2c/mips2c_table.cpp
        mips2c/functions/bones.cpp
        mips2c/functions/collide_cache.cpp
        mips2c/functions/collide_edge_grab.cpp
        mips2c/functions/collide_func.cpp
        mips2c/functions/collide_mesh.cpp
        mips2c/functions/collide_probe.cpp
        mips2c/functions/draw_string.cpp
        mips2c/functions/generic_effect.cpp
        mips2c/functions/generic_effect2.cpp
        mips2c/functions/generic_merc.cpp
        mips2c/functions/generic_tie.cpp
        mips2c/functions/joint.cpp
        mips2c/functions/merc_blend_shape.cpp
        mips2c/functions/ocean.cpp
        mips2c/functions/ocean_vu0.cpp
        mips2c/functions/ripple.cpp
        mips2c/functions/shadow.cpp
        mips2c/functions/sky_tng.cpp
        mips2c/functions/sparticle.cpp
        mips2c/functions/sparticle_launcher.cpp
        mips2c/functions/test_func.cpp
        mips2c/functions/texture.cpp
        mips2c/functions/tfrag.cpp
        mips2c/functions/tie_methods.cpp
        mips2c/functions/time_of_day.cpp
        overlord/dma.cpp
        overlord/fake_iso.cpp
        overlord/iso.cpp
        overlord/iso_api.cpp
        overlord/iso_cd.cpp
        overlord/iso_queue.cpp
        overlord/isocommon.cpp
        overlord/overlord.cpp
        overlord/ramdisk.cpp
        overlord/sbank.cpp
        overlord/soundcommon.cpp
        overlord/srpc.cpp
        overlord/ssound.cpp
        overlord/stream.cpp
        graphics/gfx.cpp
        graphics/display.cpp
        graphics/sceGraphicsInterface.cpp
        graphics/opengl_renderer/background/background_common.cpp
        graphics/opengl_renderer/background/Shrub.cpp
        graphics/opengl_renderer/background/Tfrag3.cpp
        graphics/opengl_renderer/background/TFragment.cpp
        graphics/opengl_renderer/background/Tie3.cpp
        graphics/opengl_renderer/foreground/Merc2.cpp
        graphics/opengl_renderer/foreground/Generic2.cpp
        graphics/opengl_renderer/foreground/Generic2_DMA.cpp
        graphics/opengl_renderer/foreground/Generic2_Build.cpp
        graphics/opengl_renderer/foreground/Generic2_OpenGL.cpp
        graphics/opengl_renderer/ocean/CommonOceanRenderer.cpp
        graphics/opengl_renderer/ocean/OceanMid.cpp
        graphics/opengl_renderer/ocean/OceanMid_PS2.cpp
        graphics/opengl_renderer/ocean/OceanMidAndFar.cpp
        graphics/opengl_renderer/ocean/OceanNear.cpp
        graphics/opengl_renderer/ocean/OceanNear_PS2.cpp
        graphics/opengl_renderer/ocean/OceanTexture.cpp
        graphics/opengl_renderer/ocean/OceanTexture_PC.cpp
        graphics/opengl_renderer/loader/Loader.cpp
        graphics/opengl_renderer/loader/LoaderStages.cpp
        graphics/opengl_renderer/BucketRenderer.cpp
        graphics/opengl_renderer/CollideMeshRenderer.cpp
        graphics/opengl_renderer/debug_gui.cpp
        graphics/opengl_renderer/DirectRenderer.cpp
        graphics/opengl_renderer/DirectRenderer2.cpp
        graphics/opengl_renderer/EyeRenderer.cpp
        graphics/opengl_renderer/dma_helpers.cpp
        graphics/opengl_renderer/MercProgram.cpp
        graphics/opengl_renderer/MercRenderer.cpp
        graphics/opengl_renderer/opengl_utils.cpp
        graphics/opengl_renderer/OpenGLRenderer.cpp
        graphics/opengl_renderer/Profiler.cpp
        graphics/opengl_renderer/Shader.cpp
        graphics/opengl_renderer/Shadow_PS2.cpp
        graphics/opengl_renderer/ShadowRenderer.cpp
        graphics/opengl_renderer/SkyBlendCPU.cpp
        graphics/opengl_renderer/SkyBlendGPU.cpp
        graphics/opengl_renderer/SkyRenderer.cpp
        graphics/opengl_renderer/Sprite3.cpp
        graphics/opengl_renderer/SpriteRenderer.cpp
        graphics/opengl_renderer/TextureUploadHandler.cpp
        graphics/texture/jak1_tpage_dir.cpp
        graphics/texture/TextureConverter.cpp
        graphics/texture/TexturePool.cpp
        graphics/pipelines/opengl.cpp
        system/vm/dmac.cpp
        system/vm/vm.cpp
        tools/subtitles/subtitle_editor.h
        tools/subtitles/subtitle_editor.cpp)

find_package(Git)

function(write_svnrev_h)
    set(GIT_SHORT_SHA "")
    if (GIT_FOUND AND EXISTS ${CMAKE_SOURCE_DIR}/.git)
        EXECUTE_PROCESS(WORKING_DIRECTORY ${CMAKE_SOURCE_DIR} COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
                OUTPUT_VARIABLE GIT_SHORT_SHA
                OUTPUT_STRIP_TRAILING_WHITESPACE)
    else()
        set(GIT_SHORT_SHA "unk. rev.")
    endif()
    if(NOT GIT_SHORT_SHA)
        set(GIT_SHORT_SHA "unk. rev.")
    else()
        string(SUBSTRING ${GIT_SHORT_SHA} 0 6 GIT_SHORT_SHA)
    endif()

    file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/kernel/svnrev.h "#define GIT_SHORT_SHA \"rev. ${GIT_SHORT_SHA}\"\n")
endfunction()

write_svnrev_h()

add_subdirectory(sound)

# we build the runtime as a static library.
add_library(runtime STATIC ${RUNTIME_SOURCE} "../third-party/glad/src/glad.c")

target_link_libraries(runtime common fmt glfw imgui discord-rpc sound)
if(WIN32)
    target_link_libraries(runtime mman)
else()
    target_link_libraries(runtime pthread dl)
endif()

add_executable(gk main.cpp)
target_link_libraries(gk runtime)

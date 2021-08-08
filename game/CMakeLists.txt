# We define our own compilation flags here.
set(CMAKE_CXX_STANDARD 17)

# Set default compile flags for GCC
# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
if(UNIX)
    message(STATUS "GCC detected, adding compile flags")
    set(CMAKE_CXX_FLAGS
            "${CMAKE_CXX_FLAGS} \
        -Wall \
        -Winit-self \
        -ggdb \
        -Wextra \
        -Wno-cast-align \
        -Wcast-qual \
        -Wdisabled-optimization \
        -Wformat \
        -Wmissing-include-dirs \
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo")
else()
        set(CMAKE_CXX_FLAGS "/EHsc")
endif(UNIX)

enable_language(ASM_NASM)
set(CMAKE_ASM_NASM_SOURCE_FILE_EXTENSIONS ${CMAKE_ASM_NASM_SOURCE_FILE_EXTENSIONS} asm)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> -f ${CMAKE_ASM_NASM_OBJECT_FORMAT} -o <OBJECT> <SOURCE>")
set_source_files_properties(kernel/asm_funcs.asm PROPERTIES COMPILE_FLAGS "-g")
set(RUNTIME_SOURCE
        runtime.cpp
        system/SystemThread.cpp
        system/IOP_Kernel.cpp
        system/iop_thread.cpp
        system/Deci2Server.cpp
        sce/libcdvd_ee.cpp
        sce/libscf.cpp
        sce/libdma.cpp
        sce/libpad.cpp
        sce/libgraph.cpp
        sce/deci2.cpp
        sce/sif_ee.cpp
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
        graphics/dma/dma.cpp
        graphics/gfx.cpp
        graphics/display.cpp
        graphics/sceGraphicsInterface.cpp
        graphics/dma/dma_copy.cpp
        graphics/dma/gs.cpp
        graphics/opengl_renderer/BucketRenderer.cpp
        graphics/opengl_renderer/DirectRenderer.cpp
        graphics/opengl_renderer/OpenGLRenderer.cpp
        graphics/opengl_renderer/Shader.cpp
        graphics/texture/TextureConverter.cpp
        graphics/texture/TexturePool.cpp
        system/vm/dmac.cpp
        system/vm/vm.cpp)


# we build the runtime as a static library.
add_library(runtime STATIC ${RUNTIME_SOURCE} "../third-party/glad/src/glad.c")

target_link_libraries(runtime common fmt glfw)
if(WIN32)
    target_link_libraries(runtime mman)
else()
    target_link_libraries(runtime pthread dl)
endif()

add_executable(gk main.cpp)
target_link_libraries(gk runtime)

install(TARGETS gk)

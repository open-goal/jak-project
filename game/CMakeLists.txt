# We define our own compilation flags here.
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-O0 -ggdb -Wall \
-Wextra -Wcast-align -Wcast-qual -Wdisabled-optimization -Wformat=2 \
-Winit-self -Wmissing-include-dirs -Woverloaded-virtual \
-Wredundant-decls -Wshadow -Wsign-promo ")

enable_language(ASM_NASM)
set(RUNTIME_SOURCE
        main.cpp
        runtime.cpp
        system/SystemThread.cpp
        system/IOP_Kernel.cpp
        system/iop_thread.cpp
        system/Deci2Server.cpp
        sce/libcdvd_ee.cpp
        sce/libscf.cpp
        sce/deci2.cpp
        sce/sif_ee.cpp
        sce/iop.cpp
        sce/stubs.cpp
        kernel/asm_funcs.nasm
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
        overlord/stream.cpp)

# the runtime should be built without any static/dynamic libraries.
add_executable(gk ${RUNTIME_SOURCE})

# we also build a runtime library for testing. This version is likely unable to call GOAL code correctly, but
# can be used to test other things.
add_library(runtime ${RUNTIME_SOURCE})

target_link_libraries(gk pthread)
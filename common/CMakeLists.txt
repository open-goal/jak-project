add_library(common
        SHARED
        audio/audio_formats.cpp
        cross_os_debug/xdbg.cpp
        cross_sockets/xsocket.cpp
        goos/Interpreter.cpp
        goos/Object.cpp
        goos/ParseHelpers.cpp
        goos/PrettyPrinter.cpp
        goos/Reader.cpp
        goos/TextDB.cpp
        goos/ReplUtils.cpp
        log/log.cpp
        type_system/defenum.cpp
        type_system/deftype.cpp
        type_system/state.cpp
        type_system/Type.cpp
        type_system/TypeFieldLookup.cpp
        type_system/TypeSpec.cpp
        type_system/TypeSystem.cpp
        util/BitUtils.cpp
        util/dgo_util.cpp
        util/DgoReader.cpp
        util/DgoWriter.cpp
        util/FileUtil.cpp
        util/json_util.cpp
        util/Timer.cpp
        util/print_float.cpp
        )

target_link_libraries(common fmt lzokay replxx)

if(WIN32)
    target_link_libraries(common wsock32 ws2_32)
else()
    target_link_libraries(common stdc++fs)
endif()

if(UNIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2")
elseif(WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
endif()

install(TARGETS common)

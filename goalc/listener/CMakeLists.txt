add_library(listener SHARED Listener.cpp)

IF (WIN32)
    target_link_libraries(listener cross_sockets cross_os_debug)
ELSE ()
    target_link_libraries(listener cross_sockets pthread cross_os_debug)
ENDIF ()
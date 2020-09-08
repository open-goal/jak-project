add_library(listener SHARED Listener.cpp)

IF (WIN32)
    target_link_libraries(listener cross_sockets)
ELSE ()
    target_link_libraries(listener cross_sockets pthread)
ENDIF ()
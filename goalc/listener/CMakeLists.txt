add_library(listener SHARED
        Listener.cpp)
IF (WIN32)
    #
ELSE ()
    target_link_libraries(listener pthread)
ENDIF ()
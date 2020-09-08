add_library(cross_sockets
        SHARED
        "xsocket.h"
         "xsocket.cpp")

IF (WIN32)
  # set stuff for windows
  target_link_libraries(cross_sockets wsock32 ws2_32)
ELSE()
  # set stuff for other systems
  target_link_libraries(cross_sockets)
ENDIF()

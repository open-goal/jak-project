add_library(util
        SHARED
        "FileIO.cpp")
# if you need link libraries? add this
IF (WIN32)
  # set stuff for windows
  target_link_libraries(util)
ELSE()
  # set stuff for other systems
  target_link_libraries(util)
ENDIF()
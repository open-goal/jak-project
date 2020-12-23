add_library(common_util
        SHARED
        FileUtil.cpp
        DgoWriter.cpp
        Timer.cpp)

target_link_libraries(common_util stdc++fs)

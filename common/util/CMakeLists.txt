add_library(common_util
        SHARED
        FileUtil.cpp)

target_link_libraries(common_util fmt)
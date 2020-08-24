add_library(type_system
        TypeSystem.cpp
        Type.cpp
        TypeSpec.cpp)

target_link_libraries(type_system fmt)
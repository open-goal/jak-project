add_library(type_system
        SHARED
        TypeSystem.cpp
        Type.cpp
        TypeSpec.cpp
        deftype.cpp
        TypeFieldLookup.cpp)

target_link_libraries(type_system fmt goos)
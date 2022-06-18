add_executable(build_level
        main.cpp
        collide_bvh.cpp
        color_quantization.cpp
        collide_pack.cpp
        LevelFile.cpp
        FileInfo.cpp
        gltf_mesh_extract.cpp
        Tfrag.cpp)

target_link_libraries(build_level common compiler tiny_gltf)
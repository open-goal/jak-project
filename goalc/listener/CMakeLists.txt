add_library(listener SHARED Listener.cpp)

target_link_libraries(listener cross_sockets)
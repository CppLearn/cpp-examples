cmake_minimum_required(VERSION 3.10)

# Set the project name and version
project(grid VERSION 1.0)

# Set C++11 as the minimum required standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add all .cpp files in src directory to a list
file(GLOB SOURCES "src/*.cpp" "src/*.c")
link_directories(~/github/raylib/build/raylib/external/glfw/src ~/github/raylib/build/raylib)

# Specify any additional include directories
include_directories(~/github/raylib/build/raylib/include hdr)

# Add the executable
add_executable(grid_main ${SOURCES})

target_link_libraries(grid_main raylib m atomic OpenGL GLX GLU glfw3 rt dl pthread)


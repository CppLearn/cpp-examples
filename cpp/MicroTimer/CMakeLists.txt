cmake_minimum_required(VERSION 3.10)

# Set the project name and version
project(microtimer VERSION 1.0)

# Set C++11 as the minimum required standard
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add all .cpp files in src directory to a list
file(GLOB SOURCES "src/*.cpp")

# Add the executable
add_executable(microtimer_main ${SOURCES})

# Specify any additional include directories
include_directories(hdr)




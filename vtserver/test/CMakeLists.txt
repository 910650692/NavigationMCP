# For more information about using CMake with Android Studio, read the
# documentation: https://d.android.com/studio/projects/add-native-code.html

# Sets the minimum version of CMake required to build the native library.

cmake_minimum_required(VERSION 3.10)

project("native_ut")
enable_testing()

#=========== add macro setting ============
set(CMAKE_VERBOSE_MAKEFILE ON)

#=========== add compile flags =============
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC -O3 -Wall -fexceptions")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -fPIC -O3 -Wall -fexceptions")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fPIE")

#=========== add include directories =============

include_directories(
    ${CMAKE_CURRENT_LIST_DIR}/../src/main/cpp/include
    ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/mock
)

#============= Search 3rd party libs (gtest library) =============
find_package(GTest REQUIRED)
if(NOT GTest_FOUND)
    message(FATAL_ERROR "GTest NOT FOUND")
endif(NOT GTest_FOUND)
message(STATUS "Found GTest library")
set(GTEST_LIBS GTest::GTest GTest::Main gmock)

#============= Search source code  =============
aux_source_directory(${CMAKE_CURRENT_LIST_DIR} UT_SRCS)
aux_source_directory(${CMAKE_CURRENT_LIST_DIR}/../src/main/cpp/src SERVICE_SRCS)

#============= Set  Target file  =============
set(NATIVE_UT_TARGET native_ut)
add_executable(${NATIVE_UT_TARGET} ${UT_SRCS} ${SERVICE_SRCS})
target_link_libraries(${NATIVE_UT_TARGET}
        ${GTEST_LIBS}
)

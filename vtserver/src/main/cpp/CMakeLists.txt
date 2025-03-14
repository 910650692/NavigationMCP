# For more information about using CMake with Android Studio, read the
# documentation: https://d.android.com/studio/projects/add-native-code.html

# Sets the minimum version of CMake required to build the native library.

cmake_minimum_required(VERSION 3.10)

# Declares and names the project.

project("vtserver-jni")

set( 3RDPATRY_PATH ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/vtserver)

include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${3RDPATRY_PATH}/include
)

aux_source_directory( ${CMAKE_CURRENT_SOURCE_DIR}/src SRC_VTSERVICE )
add_library( vtservice-jni SHARED com_iauto_vtserver_jni.cpp ${SRC_VTSERVICE} )

add_library( vtserver SHARED IMPORTED)
message(status "${ANDROID_ABI}")
if(ANDROID_ABI STREQUAL "armeabi-v7a")
    set_target_properties(vtserver PROPERTIES IMPORTED_LOCATION ${3RDPATRY_PATH}/lib/armeabi-v7a/libvtserver.so)
elseif(ANDROID_ABI STREQUAL "arm64-v8a")
    set_target_properties(vtserver PROPERTIES IMPORTED_LOCATION ${3RDPATRY_PATH}/lib/arm64-v8a/libvtserver.so)
endif()
find_library(
        log-lib
        log )
target_link_libraries(
        vtservice-jni
        vtserver
        ${log-lib}
)
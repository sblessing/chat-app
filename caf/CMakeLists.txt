cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
project(distributed_benchmarks)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

add_custom_target(all_benchmarks ALL)
set(CAF_COMPILED_BENCHES "")

################################################################################
#                                   Utility                                    #
################################################################################

macro(add_xxx_benchmark benchname dir name src_ending bin_ending out_path)
    set(OUT_FILE ${out_path}/${name}.${bin_ending})
    set(SRC_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/${dir}/${name}.${src_ending})
    set(CAF_${benchname}_SRC_FILES ${SRC_FILE} ${CAF_${benchname}_SRC_FILES})
    set(CAF_${benchname}_${name}_OUT ${OUT_FILE})
    set(CAF_${benchname}_OUT_FILES ${OUT_FILE} ${CAF_${benchname}_OUT_FILES})
    add_custom_command(OUTPUT ${OUT_FILE}
            COMMAND ${CAF_${benchname}_COMPILER}
            ${CAF_${benchname}_COMPILER_OPTS}
            ${SRC_FILE}
            DEPENDS ${SRC_FILE} ${ARGN}
            WORKING_DIRECTORY ${EXECUTABLE_OUTPUT_PATH}
            VERBATIM)
endmacro()

# make files show up in GUI even if not building a particular benchmark
macro(add_dummy_target benchname src_ending)
    # check all aditional dependency flags
    set(globstr "${CMAKE_CURRENT_SOURCE_DIR}/src/${benchname}/*.${src_ending}")
    foreach(additional_ending ${ARGN})
        set(globstr ${globstr}
                "${CMAKE_CURRENT_SOURCE_DIR}/src/${benchname}/*.${additional_ending}")
    endforeach()
    file(GLOB_RECURSE source_files ${globstr})
    add_custom_target(caf_${benchname}_dummy SOURCES ${source_files})
endmacro()

################################################################################
#                                     CAF                                      #
################################################################################
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_BUILD_TYPE Release)
set(CMAKE_CXX_FLAGS "-Wextra -O3 -g -Wall -pedantic -pthread")
find_package(CAF COMPONENTS core io REQUIRED)
add_custom_target(caf_benchmarks)
message(STATUS "CAF_INCLUDES: ${CAF_INCLUDE_DIRS}")
include_directories(${CAF_INCLUDE_DIRS})
# add targets for CAF benchmarks
macro(add_caf_benchmark name)
    add_executable(${name} ${CMAKE_CURRENT_SOURCE_DIR}/src/${name}.cpp)
    target_link_libraries(${name} ${CAF_LIBRARIES} ${LD_FLAGS})
    add_dependencies(caf_benchmarks ${name})
endmacro()

add_caf_benchmark(chatapp)
add_dependencies(all_benchmarks caf_benchmarks)
set(CAF_COMPILED_BENCHES "caf ${CAF_COMPILED_BENCHES}")

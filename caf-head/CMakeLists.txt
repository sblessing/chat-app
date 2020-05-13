cmake_minimum_required(VERSION 3.13.5 FATAL_ERROR)
project(chatapp_caf CXX)

set(CMAKE_CXX_STANDARD 17)

# -- includes ------------------------------------------------------------------

include(CMakePackageConfigHelpers) # For creating .cmake files
include(CheckCXXSourceCompiles)    # Check wether compiler works
include(FetchContent)              # For bundling CAF with the incubator
include(GNUInstallDirs)            # Sets default install paths
include(GenerateExportHeader)      # Auto-generates dllexport macros

# -- general options -----------------------------------------------------------

option(BUILD_SHARED_LIBS "Build shared library targets" ON)
option(CMAKE_EXPORT_COMPILE_COMMANDS "Write JSON compile commands database" ON)

# -- incubator options that are off by default ---------------------------------

option(CAF_CHATAPP_ENABLE_STANDALONE_BUILD
       "Fetch and bulid required CAF modules" OFF)

# -- incubator options with non-boolean values ---------------------------------

set(CAF_CHATAPP_SANITIZERS "" CACHE STRING
    "Comma separated sanitizers, e.g., 'address,undefined'")

# -- project-specific CMake settings -------------------------------------------

message(STATUS "Module path: ${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# --  get dependency -----------------------------------------------------------

if(CAF_CHATAPP_ENABLE_STANDALONE_BUILD)
  FetchContent_Declare(
    actor_framework
    GIT_REPOSITORY https://github.com/actor-framework/actor-framework.git
    GIT_TAG        47ebe21
  )
  FetchContent_Populate(actor_framework)
  set(CAF_ENABLE_EXAMPLES OFF CACHE BOOL "" FORCE)
  set(CAF_ENABLE_IO_MODULE OFF CACHE BOOL "" FORCE)
  set(CAF_ENABLE_TESTING OFF CACHE BOOL "" FORCE)
  set(CAF_ENABLE_TOOLS OFF CACHE BOOL "" FORCE)
  set(CAF_ENABLE_OPENSSL_MODULE OFF CACHE BOOL "" FORCE)
  set(CAF_SANITIZERS "${CAF_CHATAPP_SANITIZERS}" CACHE STRING "" FORCE)
  add_subdirectory(${actor_framework_SOURCE_DIR} ${actor_framework_BINARY_DIR})
else()
  find_package(CAF COMPONENTS core REQUIRED)
endif()

add_executable(chatapp src/chatapp.cpp)

# Flags for both compilers.
target_compile_options(chatapp PUBLIC
                       -ftemplate-depth=512 -ftemplate-backtrace-limit=0
                       -Wall -Wextra -pedantic)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # Flags for Clang only.
  target_compile_options(chatapp PUBLIC -Wdocumentation)
else()
  # Flags for GCC only.
  target_compile_options(chatapp PUBLIC -Wno-missing-field-initializers)
endif()
if(CAF_CHATAPP_SANITIZERS)
  target_compile_options(chatapp PUBLIC
                         -fsanitize=${CAF_CHATAPP_SANITIZERS}
                         -fno-omit-frame-pointer)
  target_link_libraries(chatapp PUBLIC
                        -fsanitize=${CAF_CHATAPP_SANITIZERS}
                        -fno-omit-frame-pointer)
endif()

target_include_directories(chatapp PUBLIC src/util/)
target_link_libraries(chatapp PUBLIC CAF::core)

set_target_properties(chatapp PROPERTIES OUTPUT_NAME caf-head)

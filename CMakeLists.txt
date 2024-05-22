cmake_minimum_required(VERSION 3.26)
project(store)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED yes)
set(CMAKE_CXX_EXTENSIONS  no)

find_package(Catch2 3 REQUIRED)


add_library(store INTERFACE)
add_library(store::store ALIAS store)
target_include_directories(
        store
        INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

install(TARGETS store
        EXPORT store-targets
        INCLUDES DESTINATION include)

add_executable(test_store test_store.cpp)
target_link_libraries(test_store
store::store
Catch2::Catch2WithMain)

add_executable(fuzz_store fuzz_store.cpp)
target_compile_options(fuzz_store PRIVATE "-fsanitize=fuzzer,address,undefined")
target_link_options(fuzz_store PRIVATE "-fsanitize=fuzzer,address,undefined")
target_link_libraries(fuzz_store store::store)


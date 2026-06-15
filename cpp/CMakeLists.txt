cmake_minimum_required(VERSION 3.25)
project(rabbitmq-tutorials-cpp LANGUAGES CXX)

# rmqcpp is built against C++17 (the standard its BDE dependency targets), so
# the tutorials use the same standard.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# rmqcpp is vendored as a git submodule pinned to a fixed upstream commit (see
# README.md). bal/bsl come from the BDE distribution rmqcpp is built on and are
# resolved through the vcpkg toolchain.
find_package(bal REQUIRED)
add_subdirectory(rmqcpp)

set(tutorials
    send
    receive
    new_task
    worker
    emit_log
    receive_logs
    emit_log_direct
    receive_logs_direct
    emit_log_topic
    receive_logs_topic
    rpc_server
    rpc_client
    publisher_confirms)

foreach(tutorial ${tutorials})
    add_executable(${tutorial} ${tutorial}.cpp)
    target_link_libraries(${tutorial} PRIVATE rmq bal)
endforeach()

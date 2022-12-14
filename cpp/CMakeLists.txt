set(CMAKE_CXX_STANDARD 11)

add_executable(send send.cpp)
target_link_libraries(send rabbitmq)

add_executable(receive receive.cpp)
target_link_libraries(receive rabbitmq)

add_executable(new_task new_task.cpp)
target_link_libraries(new_task rabbitmq)

add_executable(worker worker.cpp)
target_link_libraries(worker rabbitmq)

add_executable(emit_log emit_log.cpp)
target_link_libraries(emit_log rabbitmq)

add_executable(receive_logs receive_logs.cpp)
target_link_libraries(receive_logs rabbitmq)

add_executable(emit_log_direct emit_log_direct.cpp)
target_link_libraries(emit_log_direct rabbitmq)

add_executable(receive_logs_direct receive_logs_direct.cpp)
target_link_libraries(receive_logs_direct rabbitmq)

add_executable(emit_log_topic emit_log_topic.cpp)
target_link_libraries(emit_log_topic rabbitmq)

add_executable(receive_logs_topic receive_logs_topic.cpp)
target_link_libraries(receive_logs_topic rabbitmq)

add_executable(rpc_server rpc_server.cpp)
target_link_libraries(rpc_server rabbitmq)

add_executable(rpc_client rpc_client.cpp)
target_link_libraries(rpc_client rabbitmq)

add_executable(publisher_confirms publisher_confirms.cpp)
target_link_libraries(publisher_confirms rabbitmq)
  #include <iostream>
#include <string.h>
#include <algorithm>
#include <thread>
#include <chrono>

#include <amqp.h>
#include <amqp_tcp_socket.h>


int main(int argc, char const *const *argv)
{

  amqp_connection_state_t conn = amqp_new_connection();
  amqp_socket_t *socket = amqp_tcp_socket_new(conn);
  amqp_socket_open(socket, "localhost", AMQP_PROTOCOL_PORT);

  amqp_login(conn, "/", 0, AMQP_DEFAULT_FRAME_SIZE, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");
  const amqp_channel_t KChannel = 1;
  amqp_channel_open(conn, KChannel);

  amqp_bytes_t exchangeName(amqp_cstring_bytes("logs"));
  amqp_exchange_declare(conn, KChannel, exchangeName, amqp_cstring_bytes("fanout"), 
                        false, false, false, false, amqp_empty_table);

  amqp_queue_declare_ok_t *r = amqp_queue_declare(conn, KChannel, amqp_empty_bytes, false, false, true, false, amqp_empty_table);
  amqp_bytes_t queueName = amqp_bytes_malloc_dup(r->queue);

  amqp_queue_bind(conn, KChannel, queueName, exchangeName, amqp_empty_bytes, amqp_empty_table);

  std::cout << "[*] Waiting for logs. To exit press CTRL+C'" << std::endl;
  amqp_basic_consume(conn, KChannel, queueName, amqp_empty_bytes, false, /* auto ack*/true, false, amqp_empty_table);

  for (;;)
  {
    amqp_maybe_release_buffers(conn);
    amqp_envelope_t envelope;
    amqp_consume_message(conn, &envelope, nullptr, 0);

    std::string message((char *)envelope.message.body.bytes,(int)envelope.message.body.len);
    std::cout << " [x] Received " <<  message << std::endl;

    amqp_destroy_envelope(&envelope);
  }

  amqp_bytes_free(queueName);
  amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);

  return 0;
}

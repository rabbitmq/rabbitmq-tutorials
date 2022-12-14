#include <string.h>
#include <iostream>

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

  amqp_bytes_t queueName(amqp_cstring_bytes("hello"));
  amqp_queue_declare(conn, KChannel, queueName, false, false, false, false, amqp_empty_table);

  amqp_basic_publish(conn, KChannel, amqp_empty_bytes, /* routing key*/ queueName, false, false, nullptr, amqp_cstring_bytes("Hello World!"));
  std::cout << " [x] Sent 'Hello World!'" << std::endl;

  amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);
  return 0;
}

#include <string.h>
#include <iostream>
#include <sstream>
#include <iterator>

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

  amqp_bytes_t exchangeName(amqp_cstring_bytes("topic_logs"));
  amqp_exchange_declare(conn, KChannel, exchangeName, amqp_cstring_bytes("topic"), 
                        false, false, false, false, amqp_empty_table);

  std::string routing_key = argc > 2 ? argv[1] : "anonymous.info";
  std::string message(" Hello World!");
  if (argc > 2)
  {
    std::stringstream s;
    copy(&argv[2], &argv[argc], std::ostream_iterator<const char*>(s, " "));
    message = s.str();
  }

  amqp_basic_publish(conn, KChannel, exchangeName, amqp_cstring_bytes(routing_key.c_str()), false, false, nullptr, amqp_cstring_bytes(message.c_str()));
  std::cout << " [x] Sent " << routing_key << ":" << message << std::endl;

  amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);
  return 0;
}

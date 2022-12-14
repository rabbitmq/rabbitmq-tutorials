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

  amqp_bytes_t queueName(amqp_cstring_bytes("task_queue"));
  amqp_queue_declare(conn, KChannel, queueName, false, /*durable*/ true, false, true, amqp_empty_table);

  std::string message("Hello World!");
  if (argc > 1)
  {
    std::stringstream s;
    copy(&argv[1], &argv[argc], std::ostream_iterator<const char*>(s, " "));
    message = s.str();
  }

  amqp_basic_properties_t props;
  props._flags = AMQP_BASIC_DELIVERY_MODE_FLAG;
  props.delivery_mode = AMQP_DELIVERY_PERSISTENT;

  amqp_basic_publish(conn, KChannel, amqp_empty_bytes, /* routing key*/ queueName, false, false, &props, amqp_cstring_bytes(message.c_str()));
  std::cout << " [x] Sent " << message << std::endl;

  amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);
  return 0;
}

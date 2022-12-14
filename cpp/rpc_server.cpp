#include <string.h>
#include <iostream>

#include <amqp.h>
#include <amqp_tcp_socket.h>

int fib(int n)
{
  if (n == 0)
    return 0;
  else if (n == 1)
    return 1;
  else
    return fib(n-1) + fib(n-2);
}

int main(int argc, char const *const *argv)
{
  amqp_connection_state_t conn = amqp_new_connection();
  amqp_socket_t *socket = amqp_tcp_socket_new(conn);
  amqp_socket_open(socket, "localhost", AMQP_PROTOCOL_PORT);
  amqp_login(conn, "/", 0, AMQP_DEFAULT_FRAME_SIZE, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");
  const amqp_channel_t KChannel = 1;
  amqp_channel_open(conn, KChannel);

  amqp_bytes_t queueName(amqp_cstring_bytes("rpc_queue"));
  amqp_queue_declare(conn, KChannel, queueName, false, false, false, false, amqp_empty_table);

  amqp_basic_qos(conn, KChannel, 0, /*prefetch_count*/1, 0);
  amqp_basic_consume(conn, KChannel, queueName, amqp_empty_bytes, false, /* auto ack*/false, false, amqp_empty_table);
  std::cout << " [x] Awaiting RPC requests" << std::endl;

  for (;;)
  {
    amqp_maybe_release_buffers(conn);
    amqp_envelope_t envelope;
    amqp_rpc_reply_t res = amqp_consume_message(conn, &envelope, nullptr, 0);

    int n = *(int *)envelope.message.body.bytes;
    std::cout << " [.] fib(" <<  n << ")" << std::endl;
    int response = fib(n);
    amqp_bytes_t response_;
    response_.bytes = &response;
    response_.len = sizeof(response);

    amqp_basic_properties_t props;
    props._flags = AMQP_BASIC_CORRELATION_ID_FLAG;
    props.correlation_id = envelope.message.properties.correlation_id;

    amqp_channel_t replyChannel = envelope.channel;
    amqp_basic_publish(conn, replyChannel, amqp_empty_bytes, /* routing key*/ envelope.message.properties.reply_to, false, false, &props, response_);
    amqp_basic_ack(conn, replyChannel, envelope.delivery_tag, false);
    
    amqp_destroy_envelope(&envelope);
  }

  amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
  amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
  amqp_destroy_connection(conn);

  return 0;
}

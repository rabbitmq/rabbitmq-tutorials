#include <string.h>
#include <iostream>

#include <amqp.h>
#include <amqp_tcp_socket.h>

class FibonacciRpcClient
{
public:
  FibonacciRpcClient()
  {
    m_conn = amqp_new_connection();
    amqp_socket_t *socket = amqp_tcp_socket_new(m_conn);
    amqp_socket_open(socket, "localhost", AMQP_PROTOCOL_PORT);
    amqp_login(m_conn, "/", 0, AMQP_DEFAULT_FRAME_SIZE, 0, AMQP_SASL_METHOD_PLAIN, "guest", "guest");

    amqp_channel_open(m_conn, KChannel);

    amqp_queue_declare_ok_t *r = amqp_queue_declare(m_conn, KChannel, amqp_empty_bytes, false, false, false, /*auto delete*/true, amqp_empty_table);
    m_callbackQueue = amqp_bytes_malloc_dup(r->queue);
  }

  int call(int n)
  {
    m_corr_id = std::to_string(++m_requestCount);

    amqp_basic_properties_t props;
    props._flags = AMQP_BASIC_CORRELATION_ID_FLAG | AMQP_BASIC_REPLY_TO_FLAG;
    props.correlation_id = amqp_cstring_bytes(m_corr_id.c_str());
    props.reply_to = m_callbackQueue;

    amqp_bytes_t n_;
    n_.bytes = &n;
    n_.len = sizeof(n);
    amqp_basic_publish(m_conn, KChannel, amqp_empty_bytes, /* routing key*/ amqp_cstring_bytes("rpc_queue"), false, false, &props, n_);

    amqp_basic_consume(m_conn, KChannel, m_callbackQueue, amqp_empty_bytes, false, /* auto ack*/ true, false, amqp_empty_table);

    int response = 0;
    bool keepProcessing = true;
    while (keepProcessing)
    {
      amqp_maybe_release_buffers(m_conn);
      amqp_envelope_t envelope;
      amqp_consume_message(m_conn, &envelope, nullptr, 0);

      std::string correlation_id((char *)envelope.message.properties.correlation_id.bytes, (int)envelope.message.properties.correlation_id.len);
      if (correlation_id == m_corr_id)
      {
        response = *(int *)envelope.message.body.bytes;
        keepProcessing = false;
      }

      amqp_destroy_envelope(&envelope);
    }

    return response;
  }

  ~FibonacciRpcClient()
  {
    amqp_bytes_free(m_callbackQueue);
    amqp_channel_close(m_conn, KChannel, AMQP_REPLY_SUCCESS);
    amqp_connection_close(m_conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(m_conn);
  }

private:
  amqp_connection_state_t m_conn;
  const amqp_channel_t KChannel = 1;
  amqp_bytes_t m_callbackQueue;
  std::string m_corr_id;
  int m_requestCount = 0;
};

int main(int argc, char const *const *argv)
{
  int n = 30;
  if (argc > 1)
  {
    n = std::stoi(argv[1]);
  }

  FibonacciRpcClient fibonacciRpcClient;
  std::cout << " [x] Requesting fib(" << n << ")" << std::endl;
  int response = fibonacciRpcClient.call(n);
  std::cout << " [.] Got " << response << std::endl;
  return 0;
}

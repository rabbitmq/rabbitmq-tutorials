#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <amqp.h>
#include <amqp_tcp_socket.h>

int main(int argc, char const *const *argv) {
    char const *hostname;
    int port, status;
    const char* key;
    char const *exchange;
    amqp_socket_t *socket = NULL;
    amqp_connection_state_t conn;
   	const amqp_channel_t CHANNEL_ID = 1;
    const char* DEFAULT_VHOST = "/";
    const char* DEFAULT_USER = "guest";
    const char* DEFAULT_PASSWORD = "guest";
    amqp_rpc_reply_t reply;
    amqp_channel_open_ok_t* channel_status;
    amqp_exchange_declare_ok_t* exchange_status;

    if (argc != 5) {
        fprintf(stderr, "Usage: %s <host> <port> <exchange> <key>\n", argv[0]);
        return 1;
    }

    hostname = argv[1];
    port = atoi(argv[2]);
    exchange = argv[3];
    key = argv[4];

    // create new connection object
    conn = amqp_new_connection();
    if (!conn) {
        fprintf(stderr, "failed to allocate connection\n");
        return 1;
    }

    // create socket for connection
    socket = amqp_tcp_socket_new(conn);
    if (!socket) {
        fprintf(stderr, "failed to allocate tcp socket\n");
        goto error;
    }

    // use socket to open connection with broker
    status = amqp_socket_open(socket, hostname, port);
    if (status != 0) {
        fprintf(stderr, "failed to connect to broker. server error: %d errno: %d\n", status, errno);
        goto error;
    }

    // login to the broker - sync call
    reply = amqp_login(conn,
      DEFAULT_VHOST,
      AMQP_DEFAULT_MAX_CHANNELS,
      AMQP_DEFAULT_FRAME_SIZE,
      0, // no heartbeat
      AMQP_SASL_METHOD_PLAIN,
      DEFAULT_USER,
      DEFAULT_PASSWORD);
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "failed to login to broker. reply type: %d\n", reply.reply_type);
        goto error;
    }

    // open channel and check response
    channel_status = amqp_channel_open(conn, CHANNEL_ID);
    if (!channel_status) {
        fprintf(stderr, "failed to open channel\n");
        goto error;
    }

    // declare the exchange
    exchange_status = amqp_exchange_declare(
            conn, 
            CHANNEL_ID, 
            amqp_cstring_bytes(exchange),
            amqp_cstring_bytes("topic"), 
            0, // not passive - create the exchange
            1, // durable
            0, // dont autodelete
            0, // not internal
            amqp_empty_table // no attributes
            );
    if (!exchange_status) {
        fprintf(stderr, "failed to create exchange (client)\n");
        goto error;
    }
    reply = amqp_get_rpc_reply(conn);
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "failed to create exchange on broker. reply type: %d\n", reply.reply_type);
        goto error;
    }

    // send message to broker
    status = amqp_basic_publish(
        conn, 
        CHANNEL_ID, 
        amqp_cstring_bytes(exchange),
        amqp_cstring_bytes(key),
        0, // not mandatory
        0, // not immediate
        NULL, // no properties
        amqp_cstring_bytes("Hello World!")
        );
    if (status != 0) {
        fprintf(stderr, "failed to publish to broker. server error: %d errno: %d\n", status, errno);
        goto error;
    }
    reply = amqp_get_rpc_reply(conn);
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "broker reply type: %d\n", reply.reply_type);
        goto error;
    }

    printf(" [x] Sent 'Hello World!'\n");
    
    amqp_destroy_connection(conn);
    return 0;

error:
    amqp_destroy_connection(conn);
    return 1;
}


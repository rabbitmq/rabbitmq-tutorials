#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <amqp.h>
#include <amqp_tcp_socket.h>

int main(int argc, char const *const *argv) {
    char const *hostname;
    int port, status;
    char const *queue;
    amqp_socket_t *socket = NULL;
    amqp_connection_state_t conn;
   	const amqp_channel_t CHANNEL_ID = 1;
    const char* DEFAULT_VHOST = "/";
    const char* DEFAULT_USER = "guest";
    const char* DEFAULT_PASSWORD = "guest";
    amqp_rpc_reply_t reply;
    amqp_channel_open_ok_t* channel_status;
    amqp_queue_declare_ok_t* queue_status;
    amqp_basic_consume_ok_t* consume_status;

    if (argc != 4) {
        fprintf(stderr, "Usage: %s <host> <port> <queue>\n", argv[0]);
        return 1;
    }

    hostname = argv[1];
    port = atoi(argv[2]);
    queue = argv[3];

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
        return 1;
    }

    // use socket to open connection with broker
    status = amqp_socket_open(socket, hostname, port);
    if (status != 0) {
        fprintf(stderr, "failed to connect to broker. server error: %d errno: %d\n", status, errno);
        return 1;
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
        return 1;
    }

    // open channel and check response
    channel_status = amqp_channel_open(conn, CHANNEL_ID);
    if (!channel_status) {
        fprintf(stderr, "failed to open channel\n");
        return 1;
    }

    // declare a queue
    queue_status = amqp_queue_declare(conn, 
            CHANNEL_ID, 
            amqp_cstring_bytes(queue),
            0, // not passive - create the queue
            1, // durable queue
            0, // not exclusive
            0, // dont autodelete
            amqp_empty_table // no properties
            );
    if (!queue_status) {
        fprintf(stderr, "failed to create queue (client)\n");
        return 1;
    }
    reply = amqp_get_rpc_reply(conn);
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "failed to create queue on broker. reply type: %d\n", reply.reply_type);
        return 1;
    }

    // define message consumption 
    consume_status = amqp_basic_consume(
            conn, 
            CHANNEL_ID, 
            amqp_cstring_bytes(queue), 
            amqp_empty_bytes, // no consumer tag
            1, // not local 
            1, // no ack
            0, // not exclusive 
            amqp_empty_table // no attributes
            );
    if (!consume_status) {
        fprintf(stderr, "failed to start consumer (client)\n");
        return 1;
    }

    printf(" [*] Waiting for messages. To exit press CTRL+C\n");
    // processing messages
    for (;;) {
        amqp_envelope_t envelope;

        // consume message - blocking call without timeout
        reply = amqp_consume_message(conn, &envelope, NULL, 0);
        if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
            fprintf(stderr, "invalid message received. reply type: %d\n", reply.reply_type);
            break;
        }
        printf(" [x] Received %.*s\n", (int)envelope.message.body.len, (char*)envelope.message.body.bytes);
        amqp_destroy_envelope(&envelope);
    }
    // cleanup
    amqp_destroy_connection(conn);

    return 0;
}


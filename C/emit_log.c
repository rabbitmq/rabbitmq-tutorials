#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <amqp.h>
#include <amqp_tcp_socket.h>

int main(int argc, char const *const *argv) {
    char *message;
    amqp_connection_state_t conn;
    amqp_socket_t *socket = NULL;
    const char* DEFAULT_HOSTNAME = "localhost";
    const int DEFAULT_PORT = 5672;
    int socket_status;
    const char* DEFAULT_VHOST = "/";
    const char* DEFAULT_USER = "guest";
    const char* DEFAULT_PASSWORD = "guest";
    amqp_rpc_reply_t rpc_reply;
    const amqp_channel_t CHANNEL_ID = 1;
    amqp_channel_open_ok_t* channel_status;
    const char* EXCHANGE_NAME = "logs";
    const char* EXCHANGE_TYPE = "fanout";
    amqp_exchange_declare_ok_t* exchange_status;
    int publish_status;

    // Prepare message
    if (argc == 1) {
        message = "info: Hello World from C producer!";
    } else {
        int i, v = 0;
        message = (char*)malloc(v);

        for (i = 1; i < argc; i++) {
            message = (char*)realloc(message, (v + strlen(argv[i])));
            strcat(message, argv[i]);
            strcat(message, " ");
        }
    }

    // Create new connection object
    conn = amqp_new_connection();
    if (!conn) {
        fprintf(stderr, "Failed to allocate connection.\n");
        return 1;
    }

    // Create socket for connection
    socket = amqp_tcp_socket_new(conn);
    if (!socket) {
        fprintf(stderr, "Failed to allocate TCP socket.\n");
        goto error;
    }

    // Use socket to open connection with broker
    socket_status = amqp_socket_open(socket, DEFAULT_HOSTNAME, DEFAULT_PORT);
    if (socket_status != 0) {
        fprintf(stderr, "Failed to connect to broker. Server error: %d; errno: %d.\n", socket_status, errno);
        goto error;
    }

    // Login to the broker (sync call)
    rpc_reply = amqp_login(
        conn,
        DEFAULT_VHOST,
        AMQP_DEFAULT_MAX_CHANNELS,
        AMQP_DEFAULT_FRAME_SIZE,
        0, // no heartbeat
        AMQP_SASL_METHOD_PLAIN,
        DEFAULT_USER,
        DEFAULT_PASSWORD
    );
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to login to broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    // Open channel
    channel_status = amqp_channel_open(conn, CHANNEL_ID);
    if (!channel_status) {
        fprintf(stderr, "Failed to open channel.\n");
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to open channel on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    // Declare exchange
    exchange_status = amqp_exchange_declare(
        conn,
        CHANNEL_ID,
        amqp_cstring_bytes(EXCHANGE_NAME),
        amqp_cstring_bytes(EXCHANGE_TYPE),
        0, // passive
        0, // durable
        0, // auto_delete
        0, // internal
        amqp_empty_table // arguments
    );
    if (!exchange_status) {
        fprintf(stderr, "Failed to create exchange.\n");
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to create exchange on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    // Publish message
    publish_status = amqp_basic_publish(
        conn,
        CHANNEL_ID,
        amqp_cstring_bytes(EXCHANGE_NAME),
        amqp_cstring_bytes(""), // routing_key
        0, // mandatory
        0, // immediate
        NULL, // properties
        amqp_cstring_bytes(message) // body
    );
    if (publish_status != 0) {
        fprintf(stderr, "Failed to publish to broker. Server error: %d; errno: %d.\n", publish_status, errno);
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to start to consume on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    printf(" [x] Sent %s\n", message);

    amqp_destroy_connection(conn);
    return 0;

error:
    amqp_destroy_connection(conn);
    return 1;
}

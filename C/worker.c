#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#endif

#include <amqp.h>
#include <amqp_tcp_socket.h>

int main(int argc, char const *const *argv) {
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
    const char* QUEUE_NAME = "task_queue";
    amqp_queue_declare_ok_t* queue_status;
    amqp_basic_consume_ok_t* consume_status;

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

    // Declare queue
    queue_status = amqp_queue_declare(
        conn,
        CHANNEL_ID,
        amqp_cstring_bytes(QUEUE_NAME), // queue
        0, // passive
        1, // durable
        0, // exclusive
        0, // auto_delete
        amqp_empty_table // arguments
    );
    if (!queue_status) {
        fprintf(stderr, "Failed to create queue.\n");
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to create queue on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    // Spread the load equally over multiple workers
    amqp_basic_qos(
        conn,
        CHANNEL_ID,
        0, // size
        1, // count
        0  // global
    );

    // Define message consumption
    consume_status = amqp_basic_consume(
        conn,
        CHANNEL_ID,
        amqp_cstring_bytes(QUEUE_NAME),
        amqp_empty_bytes, // consumer_tag
        1, // no_local
        0, // no_ack
        0, // exclusive
        amqp_empty_table // arguments
    );
    if (!consume_status) {
        fprintf(stderr, "Failed to start to consume.\n");
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to start to consume on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    printf(" [*] Waiting for messages. To exit press CTRL+C.\n");

    // Consume messages
    for (;;) {
        amqp_envelope_t envelope;

        // Consume message (blocking call without timeout)
        rpc_reply = amqp_consume_message(conn, &envelope, NULL, 0);
        if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
            fprintf(stderr, "Invalid message received. Reply type: %d.\n", rpc_reply.reply_type);
            break;
        }

        char *body = envelope.message.body.bytes;
        int len = envelope.message.body.len;

        printf(" [x] Received %.*s\n", len, body);

        int i;
        int dots = 0;
        for (i = 0; i < len; i++) {
            if (body[i] == '.') {
                dots++;
            }
        }

        #ifdef _WIN32
        Sleep(dots);
        #else
        sleep(dots);
        #endif

        printf(" [x] Done.\n");

        amqp_basic_ack(conn, CHANNEL_ID, envelope.delivery_tag, 0);
        amqp_destroy_envelope(&envelope);
    }

    amqp_destroy_connection(conn);
    return 0;

error:
    amqp_destroy_connection(conn);
    return 1;
}

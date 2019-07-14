#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <amqp.h>
#include <amqp_tcp_socket.h>
#include <uuid/uuid.h>

int main(int argc, char const *const *argv) {
    const char *n;
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
    amqp_queue_declare_ok_t* queue_status;
    amqp_bytes_t callback_queue;
    uuid_t corr_id;
    char corr_id_string[37];
    amqp_basic_properties_t props;
    const char* DEFAULT_EXCHANGE = "";
    const char* QUEUE_NAME = "rpc_queue";
    int publish_status;
    amqp_basic_consume_ok_t* consume_status;

    if (argc > 1) {
      n = argv[1];
    } else {
      n = "30";
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

    // Declare private queue
    queue_status = amqp_queue_declare(
        conn,
        CHANNEL_ID,
        amqp_empty_bytes, // queue
        0, // passive
        0, // durable
        0, // exclusive
        1, // auto_delete
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

    // Get private queue name
    callback_queue = amqp_bytes_malloc_dup(queue_status->queue);
    if (callback_queue.bytes == NULL) {
      fprintf(stderr, "Out of memory while copying callback queue name.\n");
      return 1;
    }

    // Generate UUID to be used as correlation id
    uuid_generate(corr_id);
    uuid_unparse_lower(corr_id, corr_id_string);

    // Set message properties
    props._flags = AMQP_BASIC_CORRELATION_ID_FLAG | AMQP_BASIC_REPLY_TO_FLAG;
    props.correlation_id = amqp_cstring_bytes(corr_id_string);
    props.reply_to = amqp_bytes_malloc_dup(callback_queue);
    if (props.reply_to.bytes == NULL) {
      fprintf(stderr, "Out of memory while copying callback queue name.\n");
      return 1;
    }

    printf(" [x] Requesting fib(%s)\n", n);

    // Publish message
    publish_status = amqp_basic_publish(
        conn,
        CHANNEL_ID,
        amqp_cstring_bytes(DEFAULT_EXCHANGE),
        amqp_cstring_bytes(QUEUE_NAME), // routing_key
        0, // mandatory
        0, // immediate
        &props, // properties
        amqp_cstring_bytes(n) // body
    );
    amqp_bytes_free(props.reply_to);
    if (publish_status != 0) {
        fprintf(stderr, "Failed to publish to broker. Server error: %d; errno: %d.\n", publish_status, errno);
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to publish to broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

    // Define message consumption
    consume_status = amqp_basic_consume(
        conn,
        CHANNEL_ID,
        callback_queue,
        amqp_empty_bytes, // consumer_tag
        1, // no_local
        0, // no_ack
        0, // exclusive
        amqp_empty_table // arguments
    );
    amqp_bytes_free(callback_queue);
    if (!consume_status) {
        fprintf(stderr, "Failed to start to consume.\n");
        goto error;
    }
    rpc_reply = amqp_get_rpc_reply(conn);
    if (rpc_reply.reply_type != AMQP_RESPONSE_NORMAL) {
        fprintf(stderr, "Failed to start to consume on broker. Reply type: %d.\n", rpc_reply.reply_type);
        goto error;
    }

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

        amqp_destroy_envelope(&envelope);
        break;
    }

    amqp_destroy_connection(conn);
    return 0;

error:
    amqp_destroy_connection(conn);
    return 1;
}

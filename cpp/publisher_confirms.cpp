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

    amqp_confirm_select(conn, KChannel);
    amqp_basic_publish(conn, KChannel, amqp_empty_bytes, /* routing key*/ queueName, false, false, nullptr, amqp_cstring_bytes("Hello World!"));
    amqp_basic_publish(conn, KChannel, amqp_empty_bytes, /* routing key*/ queueName, false, false, nullptr, amqp_cstring_bytes("Hello World!"));

    amqp_frame_t frame;
    amqp_simple_wait_frame(conn, &frame);
    if (frame.channel == KChannel)
    {
        if (frame.payload.method.id == AMQP_BASIC_ACK_METHOD)
        {
            amqp_basic_ack_t *ack = (amqp_basic_ack_t *)frame.payload.method.decoded;
            if (ack->multiple)
                std::cout << "Sucessfully sent messages up to delivery tag: " << ack->delivery_tag << std::endl;
            else
                std::cout << "Sucessfully sent message with delivery tag: " << ack->delivery_tag << std::endl;
        }
        else if (frame.payload.method.id == AMQP_BASIC_RETURN_METHOD)
        {
            // message wasn't routed to a queue, but returned
            amqp_message_t returned_message;
            amqp_read_message(conn, 1, &returned_message, 0);
            amqp_destroy_message(&returned_message);

            amqp_simple_wait_frame(conn, &frame);
            if (frame.payload.method.id == AMQP_BASIC_ACK_METHOD)
                std::cout << "Message returned" << std::endl;
        }
    }

    amqp_channel_close(conn, KChannel, AMQP_REPLY_SUCCESS);
    amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(conn);
    return 0;
}

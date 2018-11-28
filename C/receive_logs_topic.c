/*
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MIT
 *
 * Portions created by Alan Antonuk are Copyright (c) 2012-2013
 * Alan Antonuk. All Rights Reserved.
 *
 * Portions created by VMware are Copyright (c) 2007-2012 VMware, Inc.
 * All Rights Reserved.
 *
 * Portions created by Tony Garnock-Jones are Copyright (c) 2009-2010
 * VMware, Inc. and Tony Garnock-Jones. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * ***** END LICENSE BLOCK *****
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <amqp.h>
#include <amqp_tcp_socket.h>

#include <assert.h>

#include "utils.h"

static void run(amqp_connection_state_t conn) {

    amqp_frame_t frame;

    for (;;) {
        amqp_rpc_reply_t ret;
        amqp_envelope_t envelope;

        // TODO why is it needed?
        amqp_maybe_release_buffers(conn);
        // consume message - blocking call without timeout
        ret = amqp_consume_message(conn, &envelope, NULL, 0);

        // check the received envelope
        if (AMQP_RESPONSE_NORMAL != ret.reply_type) {
            if (AMQP_RESPONSE_LIBRARY_EXCEPTION == ret.reply_type &&
                    AMQP_STATUS_UNEXPECTED_STATE == ret.library_error) {
                if (AMQP_STATUS_OK != amqp_simple_wait_frame(conn, &frame)) {
                    return;
                }

                if (AMQP_FRAME_METHOD == frame.frame_type) {
                    switch (frame.payload.method.id) {
                        case AMQP_BASIC_ACK_METHOD:
                            /* if we've turned publisher confirms on, and we've published a
                             * message here is a message being confirmed.
                             */
                            break;
                        case AMQP_BASIC_RETURN_METHOD:
                            /* if a published message couldn't be routed and the mandatory
                             * flag was set this is what would be returned. The message then
                             * needs to be read.
                             */
                            {
                                amqp_message_t message;
                                ret = amqp_read_message(conn, frame.channel, &message, 0);
                                if (AMQP_RESPONSE_NORMAL != ret.reply_type) {
                                    return;
                                }

                                amqp_destroy_message(&message);
                            }

                            break;

                        case AMQP_CHANNEL_CLOSE_METHOD:
                            /* a channel.close method happens when a channel exception occurs,
                             * this can happen by publishing to an exchange that doesn't exist
                             * for example.
                             *
                             * In this case you would need to open another channel redeclare
                             * any queues that were declared auto-delete, and restart any
                             * consumers that were attached to the previous channel.
                             */
                            return;

                        case AMQP_CONNECTION_CLOSE_METHOD:
                            /* a connection.close method happens when a connection exception
                             * occurs, this can happen by trying to use a channel that isn't
                             * open for example.
                             *
                             * In this case the whole connection must be restarted.
                             */
                            return;

                        default:
                            fprintf(stderr, "An unexpected method was received %u\n",
                                    frame.payload.method.id);
                            return;
                    }
                }
            }

        } else {
            // normal message was received
            printf("Message was recevied\n");
            amqp_dump(envelope.message.body.bytes, envelope.message.body.len);
            amqp_destroy_envelope(&envelope);
        }
    }
}

int main(int argc, char const *const *argv) {
    char const *hostname;
    int port, status;
    char const *exchange;
    char const *bindingkey;
    amqp_socket_t *socket = NULL;
    amqp_connection_state_t conn;
   	const amqp_channel_t CHANNEL_ID = 1;
    amqp_bytes_t queuename;

    if (argc != 5) {
        fprintf(stderr, "Usage: %s <host> <port> <exchange> <bindkey>\n", argv[0]);
        return 1;
    }

    hostname = argv[1];
    port = atoi(argv[2]);
    exchange = argv[3];
    bindingkey = argv[4];

    // create new connection object
    conn = amqp_new_connection();

    // create socket for connection
    socket = amqp_tcp_socket_new(conn);
    if (!socket) {
        die("creating TCP socket");
    }

    // use socket to open connection with broker
    status = amqp_socket_open(socket, hostname, port);
    if (status) {
        die("opening TCP socket");
    }

    // login to the broker - TODO: check if needed
    die_on_amqp_error(amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN,
                "guest", "guest"),
            "Logging in");
    // open channel and check response
    amqp_channel_open(conn, CHANNEL_ID);
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Opening channel");

    // create a temporary queue queue that will be automatically
    // deleted when client ends
    {
        amqp_queue_declare_ok_t *r = amqp_queue_declare(
                conn, 
                CHANNEL_ID, 
                amqp_empty_bytes, // q name will be generated by broker
                0, // not passive TODO: what is that?
                0, // not durable
                1, // exclusive
                1, // auto delete
                amqp_empty_table // no attributes
                );
        die_on_amqp_error(amqp_get_rpc_reply(conn), "Declaring queue");
        // store the generated name
        queuename = amqp_bytes_malloc_dup(r->queue);
        if (queuename.bytes == NULL) {
            die("Out of memory while copying queue name");
        }
    }

    // declare the exchange
    amqp_exchange_declare(
            conn, 
            CHANNEL_ID, 
            amqp_cstring_bytes(exchange),
            amqp_cstring_bytes("topic"), 
            0, // not passive TODO: ?
            0, // not durable TODO ?
            0, // dont auto-delete
            0, // internal TODO ?
            amqp_empty_table // no attributes
            );
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Declaring exchange");

    // bind queue to key
    amqp_queue_bind(conn, CHANNEL_ID, queuename, amqp_cstring_bytes(exchange),
            amqp_cstring_bytes(bindingkey), amqp_empty_table);
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Binding queue");

    // define message consumption 
    amqp_basic_consume(
            conn, 
            CHANNEL_ID, 
            queuename, 
            amqp_empty_bytes, // no consumer tag
            0, // local TODO: what is local?
            1, // no ack
            0, // not exclusive TODO: what is exclusive?
            amqp_empty_table // no attributes
            );
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Consuming");

    // processing messages
    run(conn);

    // cleanup
    die_on_amqp_error(amqp_channel_close(conn, CHANNEL_ID, AMQP_REPLY_SUCCESS),
            "Closing channel");
    die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS),
            "Closing connection");
    die_on_error(amqp_destroy_connection(conn), "Ending connection");

    return 0;
}


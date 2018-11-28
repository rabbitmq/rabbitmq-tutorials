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

int main(int argc, char const *const *argv) {
    char const *hostname;
    int port, status;
    char const *exchange;
    char const *bindingkey;
    amqp_socket_t *socket = NULL;
    amqp_connection_state_t conn;
   	const amqp_channel_t CHANNEL_ID = 1;
    const char* DEFAULT_VHOST = "/";

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

    // login to the broker
    die_on_amqp_error(
            amqp_login(
                conn, 
                DEFAULT_VHOST, 
                0, 
                131072, 
                0, 
                AMQP_SASL_METHOD_PLAIN,
                "guest", 
                "guest"
                ),
            "Logging in");

    // open channel and check response
    amqp_channel_open(conn, CHANNEL_ID);
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Opening channel");

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

    // send message to broker
    amqp_basic_publish(
            conn, 
            CHANNEL_ID, 
            amqp_cstring_bytes(exchange),
            amqp_cstring_bytes(bindingkey),
            0, // not mandatory
            0, // not immediate
            NULL, // no properties
            amqp_cstring_bytes("Hello World")
            );
    die_on_amqp_error(amqp_get_rpc_reply(conn), "Sending message");

    // cleanup
    die_on_amqp_error(amqp_channel_close(conn, CHANNEL_ID, AMQP_REPLY_SUCCESS),
            "Closing channel");
    die_on_amqp_error(amqp_connection_close(conn, AMQP_REPLY_SUCCESS),
            "Closing connection");
    die_on_error(amqp_destroy_connection(conn), "Ending connection");

    return 0;
}


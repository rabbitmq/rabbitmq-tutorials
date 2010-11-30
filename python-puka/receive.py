#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
ticket = client.connect()
client.wait(ticket)


ticket = client.queue_declare(queue='test')
client.wait(ticket)

print ' [*] Waiting for messages. To exit press CTRL+C'

consume_ticket = client.basic_consume(queue='test',
                                      no_ack=True)
while True:
    msg_result = client.wait(consume_ticket)
    print " [x] Received %r" % (msg_result['body'],)

#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.queue_declare(queue='hello')
client.wait(promise)


print ' [*] Waiting for messages. To exit press CTRL+C'

consume_promise = client.basic_consume(queue='hello', no_ack=True)
while True:
    msg_result = client.wait(consume_promise)
    print " [x] Received %r" % (msg_result['body'],)

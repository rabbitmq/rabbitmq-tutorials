#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.exchange_declare(exchange='logs', type='fanout')
client.wait(promise)

promise = client.queue_declare(exclusive=True)
queue_name = client.wait(promise)['queue']

promise = client.queue_bind(exchange='logs', queue=queue_name)
client.wait(promise)


print ' [*] Waiting for logs. To exit press CTRL+C'

consume_promise = client.basic_consume(queue=queue_name, no_ack=True)
while True:
    msg_result = client.wait(consume_promise)
    print " [x] %r" % (msg_result['body'],)

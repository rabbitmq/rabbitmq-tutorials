#!/usr/bin/env python
import puka
import time

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.queue_declare(queue='task_queue', durable=True)
client.wait(promise)
print ' [*] Waiting for messages. To exit press CTRL+C'

consume_promise = client.basic_consume(queue='task_queue', prefetch_count=1)
while True:
    msg_result = client.wait(consume_promise)
    body = msg_result['body']
    print " [x] Received %r" % (body,)
    time.sleep( body.count('.') )
    print " [x] Done"
    client.basic_ack(msg_result)

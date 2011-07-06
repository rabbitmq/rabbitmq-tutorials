#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.queue_declare(queue='task_queue', durable=True)
client.wait(promise)

message = ' '.join(sys.argv[1:]) or "Hello World!"
promise = client.basic_publish(exchange='',
                               routing_key='task_queue',
                               body=message,
                               headers={'delivery_mode': 2})
client.wait(promise)
print " [x] Sent %r" % (message,)

client.close()

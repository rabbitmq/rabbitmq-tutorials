#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
ticket = client.connect()
client.wait(ticket)

ticket = client.queue_declare(queue='task_queue')
client.wait(ticket)

message = ' '.join(sys.argv[1:]) or "Hello World!"
ticket = client.basic_publish(exchange='',
                              routing_key='task_queue',
                              body=message)
client.wait(ticket)
print " [x] Sent %r" % (message,)


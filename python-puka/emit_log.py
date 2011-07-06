#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.exchange_declare(exchange='logs', type='fanout')
client.wait(promise)

message = ' '.join(sys.argv[1:]) or "info: Hello World!"
promise = client.basic_publish(exchange='logs', routing_key='', body=message)
client.wait(promise)

print " [x] Sent %r" % (message,)
client.close()

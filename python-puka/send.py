#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.queue_declare(queue='hello')
client.wait(promise)

promise = client.basic_publish(exchange='',
                               routing_key='hello',
                               body="Hello World!")
client.wait(promise)

print " [x] Sent 'Hello World!'"
client.close()

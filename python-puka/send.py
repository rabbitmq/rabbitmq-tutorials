#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
ticket = client.connect()
client.wait(ticket)

ticket = client.queue_declare(queue='hello')
client.wait(ticket)

ticket = client.basic_publish(exchange='',
                              routing_key='hello',
                              body="Hello world!")
client.wait(ticket)


print " [x] Sent 'Hello World!'"

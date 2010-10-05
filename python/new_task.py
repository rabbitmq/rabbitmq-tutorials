#!/usr/bin/env python
import pika
import sys

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()

channel.queue_declare(queue='test_dur', durable=True)

message = ' '.join(sys.argv[1:]) or "Hello World!"
channel.basic_publish(exchange='', routing_key='test',
                      body=message,
                      properties=pika.BasicProperties(
                         delivery_mode = 2, # make message persistent
                      ))
print " [x] Sent %r" % (message,)

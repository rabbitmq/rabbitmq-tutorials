#!/usr/bin/env python
import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()


channel.queue_declare(queue='test')

channel.basic_publish(exchange='',
                      routing_key='test',
                      body='Hello World!')
print " [x] Sent 'Hello World!'"
connection.close()

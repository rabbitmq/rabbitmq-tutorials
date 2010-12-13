#!/usr/bin/env python
import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='logs',
                         type='fanout')

result = channel.queue_declare(auto_delete=True)
queue_name = result.queue

channel.queue_bind(exchange='logs',
                   queue=queue_name)

print ' [*] Waiting for logs. To exit press CTRL+C'

def callback(ch, method, header, body):
    print " [x] %r" % (body,)

channel.basic_consume(callback,
                      queue=queue_name,
                      no_ack=True)

pika.asyncore_loop()

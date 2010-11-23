#!/usr/bin/env python
import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()


channel.queue_declare(queue='test')

print ' [*] Waiting for messages. To exit press CTRL+C'

def callback(ch, method, header, body):
    print " [x] Received %r" % (body,)

channel.basic_consume(callback,
                      queue='test',
                      no_ack=True)

pika.asyncore_loop()

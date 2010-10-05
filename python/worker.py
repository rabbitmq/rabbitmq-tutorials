#!/usr/bin/env python
import pika
import time

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()

channel.queue_declare(queue='test')
print ' [*] Waiting for messages. To exit press CTRL+C'

def callback(ch, method, header, body):
    print " [x] Received %r" % (body,)
    time.sleep( body.count('.') )
    print " [x] Done"
    ch.basic_ack(delivery_tag = method.delivery_tag)

channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback,
                      queue='test')

pika.asyncore_loop()


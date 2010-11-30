#!/usr/bin/env python
import json
import pika

def fib(n):
   if n == 0:
      return 0
   elif n == 1:
      return 1
   else:
      return fib(n-1) + fib(n-2)


class FibonacciServer(object):
    def __init__(self):
        self.connection = pika.AsyncoreConnection(pika.ConnectionParameters(
                host='127.0.0.1',
                credentials=pika.PlainCredentials('guest', 'guest')))
        self.channel = self.connection.channel()

        self.channel.queue_declare(queue='rpc_queue')

        self.channel.basic_qos(prefetch_count=1)
        self.channel.basic_consume(self.on_request, queue='rpc_queue')

    def on_request(self, ch, method, props, body):
        args, kwargs = json.loads(body)
        response = self.called(*args, **kwargs)

        ch.basic_publish(exchange='',
                         routing_key=props.reply_to,
                         properties=pika.BasicProperties(correlation_id = \
                                                         props.correlation_id),
                         body=json.dumps([response]))
        ch.basic_ack(delivery_tag = method.delivery_tag)

    def called(self, n):
        print " [.] fib(%s)"  % (n,)
        return fib(n)


srv = FibonacciServer()
print " [x] Awaiting RPC requests"
pika.asyncore_loop()

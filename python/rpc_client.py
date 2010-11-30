#!/usr/bin/env python
import json
import pika
import random

class FibonacciClient(object):
    def __init__(self):
        self.connection = pika.AsyncoreConnection(pika.ConnectionParameters(
                host='127.0.0.1',
                credentials=pika.PlainCredentials('guest', 'guest')))
        self.channel = self.connection.channel()

        result = self.channel.queue_declare(auto_delete=True)
        self.callback_queue = result.queue

    def call(self, *args, **kwargs):
        correlation_id = str(random.random())
        self.channel.basic_publish(exchange='',
                                   routing_key='rpc_queue',
                                   properties=pika.BasicProperties(
                                         reply_to = self.callback_queue,
                                         correlation_id = correlation_id,
                                         ),
                                   body=json.dumps([args, kwargs]))
        response = []
        def on_basic_deliver(ch, method, props, body):
            if props.correlation_id == correlation_id:
                response.append(body)
        self.channel.basic_consume(on_basic_deliver,
                                   queue=self.callback_queue,
                                   no_ack=True)
        while not response:
            pika.asyncore_loop(count=1)

        return json.loads(response[0])[0]


rpc = FibonacciClient()

print " [x] Requesting fib(33)"
response = rpc.call(33)
print " [.] Got %r" % (response,)


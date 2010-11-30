#!/usr/bin/env python
import pika
import uuid

class FibonacciClient(object):
    def __init__(self):
        self.connection = pika.AsyncoreConnection(pika.ConnectionParameters(
                host='127.0.0.1',
                credentials=pika.PlainCredentials('guest', 'guest')))
        self.channel = self.connection.channel()

        result = self.channel.queue_declare(auto_delete=True)
        self.callback_queue = result.queue

    def call(self, n):
        correlation_id = str(uuid.uuid4())
        self.channel.basic_publish(exchange='',
                                   routing_key='rpc_queue',
                                   properties=pika.BasicProperties(
                                         reply_to = self.callback_queue,
                                         correlation_id = correlation_id,
                                         ),
                                   body=str(n))
        response = []
        def on_basic_deliver(ch, method, props, body):
            if props.correlation_id == correlation_id:
                response.append(body)
        self.channel.basic_consume(on_basic_deliver,
                                   queue=self.callback_queue,
                                   no_ack=True)
        while not response:
            pika.asyncore_loop(count=1)

        return int(response[0])


fibonacci_rpc = FibonacciClient()

print " [x] Requesting fib(30)"
response = fibonacci_rpc.call(30)
print " [.] Got %r" % (response,)


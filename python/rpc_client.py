#!/usr/bin/env python
import pika
import uuid

class FibonacciRpcClient(object):
    def __init__(self):
        self.connection = pika.AsyncoreConnection(pika.ConnectionParameters(
                host='localhost'))

        self.channel = self.connection.channel()

        result = self.channel.queue_declare(exclusive=True)
        self.callback_queue = result.queue

        self.requests = {}
        self.channel.basic_consume(self.on_response, no_ack=True,
                                   queue=self.callback_queue)

    def on_response(self, ch, method, props, body):
        corr_id = props.correlation_id
        if corr_id in self.requests:
            self.requests[corr_id] = body

    def call(self, n):
        corr_id = str(uuid.uuid4())
        self.requests[corr_id] = None
        self.channel.basic_publish(exchange='',
                                   routing_key='rpc_queue',
                                   properties=pika.BasicProperties(
                                         reply_to = self.callback_queue,
                                         correlation_id = corr_id,
                                         ),
                                   body=str(n))
        while self.requests[corr_id] is None:
            pika.asyncore_loop(count=1)
        response = self.requests[corr_id]
        del self.requests[corr_id]
        return int(response)


fibonacci_rpc = FibonacciRpcClient()

print " [x] Requesting fib(30)"
response = fibonacci_rpc.call(30)
print " [.] Got %r" % (response,)


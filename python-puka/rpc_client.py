#!/usr/bin/env python
import puka
import uuid

class FibonacciRpcClient(object):
    def __init__(self):
        self.client = client = puka.Client("amqp://localhost/")
        promise = client.connect()
        client.wait(promise)

        promise = client.queue_declare(exclusive=True)
        self.callback_queue = client.wait(promise)['queue']

        self.consume_promise = client.basic_consume(queue=self.callback_queue,
                                                    no_ack=True)

    def call(self, n):
        correlation_id = str(uuid.uuid4())
        # We don't need to wait on promise from publish, let it happen async.
        self.client.basic_publish(exchange='',
                                  routing_key='rpc_queue',
                                  headers={'reply_to': self.callback_queue,
                                           'correlation_id': correlation_id},
                                  body=str(n))
        while True:
            msg_result = self.client.wait(self.consume_promise)
            if msg_result['headers']['correlation_id'] == correlation_id:
                return int(msg_result['body'])


fibonacci_rpc = FibonacciRpcClient()

print " [x] Requesting fib(30)"
response = fibonacci_rpc.call(30)
print " [.] Got %r" % (response,)

#!/usr/bin/env python
import puka

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)

promise = client.queue_declare(queue='rpc_queue')
client.wait(promise)

# The worlds worst algorithm:
def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)


print " [x] Awaiting RPC requests"
consume_promise = client.basic_consume(queue='rpc_queue', prefetch_count=1)
while True:
    msg_result = client.wait(consume_promise)
    n = int(msg_result['body'])

    print " [.] fib(%s)"  % (n,)
    response = fib(n)

    # This publish doesn't need to be synchronous.
    client.basic_publish(exchange='',
                         routing_key=msg_result['headers']['reply_to'],
                         headers={'correlation_id':
                                      msg_result['headers']['correlation_id']},
                         body=str(response))
    client.basic_ack(msg_result)

#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.exchange_declare(exchange='direct_logs', type='direct')
client.wait(promise)

promise = client.queue_declare(exclusive=True)
queue_name = client.wait(promise)['queue']

severities = sys.argv[1:]
if not severities:
    print >> sys.stderr, "Usage: %s [info] [warning] [error]" % (sys.argv[0],)
    sys.exit(1)

for severity in severities:
    promise = client.queue_bind(exchange='direct_logs', queue=queue_name,
                                routing_key=severity)
    client.wait(promise)


print ' [*] Waiting for logs. To exit press CTRL+C'

consume_promise = client.basic_consume(queue=queue_name, no_ack=True)
while True:
    msg_result = client.wait(consume_promise)
    print " [x] %r:%r" % (msg_result['routing_key'], msg_result['body'])

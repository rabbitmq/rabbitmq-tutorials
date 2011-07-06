#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.exchange_declare(exchange='direct_logs', type='direct')
client.wait(promise)

severity = sys.argv[1] if len(sys.argv) > 1 else 'info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'
promise = client.basic_publish(exchange='direct_logs', routing_key=severity,
                               body=message)
client.wait(promise)

print " [x] Sent %r:%r" % (severity, message)
client.close()

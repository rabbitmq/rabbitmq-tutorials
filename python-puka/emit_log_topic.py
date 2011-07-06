#!/usr/bin/env python
import puka
import sys

client = puka.Client("amqp://localhost/")
promise = client.connect()
client.wait(promise)


promise = client.exchange_declare(exchange='topic_logs', type='topic')
client.wait(promise)

routing_key = sys.argv[1] if len(sys.argv) > 1 else 'anonymous.info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'
promise = client.basic_publish(exchange='topic_logs', routing_key=routing_key,
                               body=message)
client.wait(promise)

print " [x] Sent %r:%r" % (routing_key, message)
client.close()

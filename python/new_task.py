#!/usr/bin/env python
import pika
import sys

# Establish a thread-safe connection.
connection = pika.Connection(
    pika.ConnectionParameters(host='localhost')
)

channel = connection.channel()

# Declare a durable quorum queue.
channel.queue_declare(
    queue='task_queue',
    durable=True,
    arguments={'x-queue-type': 'quorum'}
)

# Read the task from command-line arguments.
message = ' '.join(sys.argv[1:]) or "Hello World!"

# Publish the message with delivery_mode=2
# so it survives a broker restart.
channel.basic_publish(
    exchange='',
    routing_key='task_queue',
    body=message,
    properties=pika.BasicProperties(
        delivery_mode=2
    )
)

print(f" [x] Sent {message}")

connection.close()
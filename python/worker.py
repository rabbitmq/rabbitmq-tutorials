#!/usr/bin/env python
import pika
import time
import threading

connection = pika.Connection(
    pika.ConnectionParameters(host='localhost')
)

channel = connection.channel()

channel.queue_declare(
    queue='task_queue',
    durable=True,
    arguments={'x-queue-type': 'quorum'}
)

print(' [*] Waiting for messages. To exit press CTRL+C')


def callback(ch, method, properties, body):
    print(f" [x] Received {body.decode()}")

    time.sleep(body.count(b'.'))

    print(" [x] Done")

    # Safe to acknowledge directly from this callback.
    ch.basic_ack(delivery_tag=method.delivery_tag)


channel.basic_qos(prefetch_count=1)

channel.basic_consume(
    queue='task_queue',
    on_message_callback=callback
)

try:
    # Connection runs its I/O loop in a background thread.
    # Keep the main thread alive while consuming.
    threading.Event().wait()

except KeyboardInterrupt:
    print("Stopping worker...")
    connection.close()
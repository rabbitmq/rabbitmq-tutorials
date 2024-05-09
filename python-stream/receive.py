import asyncio
import signal

from rstream import (
    AMQPMessage,
    Consumer,
    MessageContext,
    ConsumerOffsetSpecification,
    OffsetType,
)

STREAM_NAME = "hello-python-stream"
# 5GB
STREAM_RETENTION = 5000000000


async def receive():
    consumer = Consumer(host="localhost", username="guest", password="guest")
    await consumer.create_stream(
        STREAM_NAME, exists_ok=True, arguments={"MaxLengthBytes": STREAM_RETENTION}
    )

    loop = asyncio.get_event_loop()
    loop.add_signal_handler(
        signal.SIGINT, lambda: asyncio.create_task(consumer.close())
    )

    async def on_message(msg: AMQPMessage, message_context: MessageContext):
        stream = message_context.consumer.get_stream(message_context.subscriber_name)
        print("Got message: {} from stream {}".format(msg, stream))

    print("Press control +C to close")
    await consumer.start()
    await consumer.subscribe(
        stream=STREAM_NAME,
        callback=on_message,
        offset_specification=ConsumerOffsetSpecification(OffsetType.FIRST, None),
    )
    await consumer.run()
    # give time to the consumer task to close the consumer
    await asyncio.sleep(1)


asyncio.run(receive())

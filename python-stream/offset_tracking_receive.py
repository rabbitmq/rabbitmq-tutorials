import asyncio

from rstream import (
    AMQPMessage,
    Consumer,
    ConsumerOffsetSpecification,
    MessageContext,
    OffsetNotFound,
    OffsetType,
    ServerError,
    amqp_decoder,
)

message_count = -1
first_offset = -1
last_offset = -1
STREAM_NAME = "stream-offset-tracking-python"
# 2GB
STREAM_RETENTION = 2000000000


async def on_message(msg: AMQPMessage, message_context: MessageContext):
    global message_count
    global first_offset
    global last_offset

    offset = message_context.offset
    if first_offset == -1:
        print("First message received")
        first_offset = offset

    consumer = message_context.consumer
    stream = message_context.consumer.get_stream(message_context.subscriber_name)

    # store the offset after every 10 messages received
    message_count = message_count + 1

    if message_count % 10 == 0:
        await consumer.store_offset(
            stream=stream,
            offset=offset,
            subscriber_name=message_context.subscriber_name,
        )

    if "marker" in str(msg):
        await consumer.store_offset(
            stream=stream,
            offset=offset,
            subscriber_name=message_context.subscriber_name,
        )
        last_offset = offset
        await consumer.close()


async def consume():
    stored_offset = -1
    global first_offset
    global last_offset

    consumer = Consumer(
        host="localhost",
        port=5552,
        username="guest",
        password="guest",
    )

    await consumer.create_stream(
        STREAM_NAME, exists_ok=True, arguments={"max-length-bytes": STREAM_RETENTION}
    )

    try:
        await consumer.start()
        print("Started consuming: Press control +C to close")
        try:
            # will raise an exception if store_offset wasn't invoked before
            stored_offset = await consumer.query_offset(
                stream=STREAM_NAME, subscriber_name="subscriber_1"
            )
        except OffsetNotFound as offset_exception:
            print(f"Offset not previously stored. {offset_exception}")

        except ServerError as server_error:
            print(f"Server error: {server_error}")
            exit(1)

        # if no offset was previously stored start from the first offset
        stored_offset = stored_offset + 1
        await consumer.subscribe(
            stream=STREAM_NAME,
            subscriber_name="subscriber_1",
            callback=on_message,
            decoder=amqp_decoder,
            offset_specification=ConsumerOffsetSpecification(
                OffsetType.OFFSET, stored_offset
            ),
        )
        await consumer.run()

    except (KeyboardInterrupt, asyncio.exceptions.CancelledError):
        await consumer.close()

    # give time to the consumer task to close the consumer
    await asyncio.sleep(1)

    if first_offset != -1:
        print(
            "Done consuming first_offset: {} last_offset {} ".format(
                first_offset, last_offset
            )
        )


with asyncio.Runner() as runner:
    runner.run(consume())

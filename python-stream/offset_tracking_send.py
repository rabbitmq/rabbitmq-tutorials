import asyncio

from rstream import AMQPMessage, ConfirmationStatus, Producer

STREAM = "stream-offset-tracking-python"
MESSAGES = 100
# 2GB
STREAM_RETENTION = 2000000000
confirmed_messages = 0
all_confirmed_messages_cond = asyncio.Condition()


async def _on_publish_confirm_client(confirmation: ConfirmationStatus) -> None:
    global confirmed_messages
    if confirmation.is_confirmed:
        confirmed_messages = confirmed_messages + 1
        if confirmed_messages == 100:
            async with all_confirmed_messages_cond:
                all_confirmed_messages_cond.notify()


async def publish():
    async with Producer("localhost", username="guest", password="guest") as producer:
        # create a stream if it doesn't already exist
        await producer.create_stream(
            STREAM, exists_ok=True, arguments={"max-length-bytes": STREAM_RETENTION}
        )

        print("Publishing {} messages".format(MESSAGES))
        # Send 99 hello message
        for i in range(MESSAGES - 1):
            amqp_message = AMQPMessage(
                body=bytes("hello: {}".format(i), "utf-8"),
            )

            await producer.send(
                stream=STREAM,
                message=amqp_message,
                on_publish_confirm=_on_publish_confirm_client,
            )
        # Send a final marker message
        amqp_message = AMQPMessage(
            body=bytes("marker: {}".format(i + 1), "utf-8"),
        )

        await producer.send(
            stream=STREAM,
            message=amqp_message,
            on_publish_confirm=_on_publish_confirm_client,
        )

        async with all_confirmed_messages_cond:
            await all_confirmed_messages_cond.wait()

        print("Messages confirmed: true")


asyncio.run(publish())

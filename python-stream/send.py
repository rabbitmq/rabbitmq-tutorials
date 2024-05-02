import asyncio

from rstream import Producer

STREAM_NAME = "hello-python-stream"
# 5GB
STREAM_RETENTION = 5000000000


async def send():
    async with Producer(
        host="localhost",
        username="guest",
        password="guest",
    ) as producer:
        await producer.create_stream(
            STREAM_NAME, exists_ok=True, arguments={"MaxLengthBytes": STREAM_RETENTION}
        )

        await producer.send(stream=STREAM_NAME, message=b"Hello, World!")

        print(" [x] Hello, World! message sent")

        input(" [x] Press Enter to close the producer  ...")


asyncio.run(send())

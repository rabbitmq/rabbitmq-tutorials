const rabbit = require("rabbitmq-stream-js-client")

const sleep = (ms) => new Promise((r) => setTimeout(r, ms))

async function main() {
    const streamName = "test-queue-stream"

    console.log("Connecting...");
    const client = await rabbit.connect({
        hostname: "localhost",
        port: 5552,
        username: "rabbit",
        password: "rabbit",
        vhost: "/",
    })

    console.log("Making sure the stream exists...");
    const streamSizeRetention = 5 * 1e9
    await client.createStream({ stream: streamName, arguments: { "max-length-bytes": streamSizeRetention } });

    console.log("Declaring the consumer with offset...");
    await client.declareConsumer({ stream: streamName, offset: rabbit.Offset.first() }, (message) => {
        console.log(`Received message ${message.content.toString()}`)
    })

    await sleep(2000)

    console.log("Closing the connection...");
    await client.close()
}

main()
    .then(() => console.log("done!"))
    .catch((res) => {
        console.log("Error while receiving message!", res)
        process.exit(-1)
    })

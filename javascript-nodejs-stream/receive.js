const rabbit = require("rabbitmq-stream-js-client")

async function main() {
    const streamName = "hello-nodejs-stream"

    console.log("Connecting...");
    const client = await rabbit.connect({
        hostname: "localhost",
        port: 5552,
        username: "guest",
        password: "guest",
        vhost: "/",
    })

    console.log("Making sure the stream exists...");
    const streamSizeRetention = 5 * 1e9
    await client.createStream({ stream: streamName, arguments: { "max-length-bytes": streamSizeRetention } });

    console.log("Declaring the consumer with offset...");
    await client.declareConsumer({ stream: streamName, offset: rabbit.Offset.first() }, (message) => {
        console.log(`Received message ${message.content.toString()}`)
    })

}

main()
    .then(async () => {
        await new Promise(function () { })
    })
    .catch((res) => {
        console.log("Error while receiving message!", res)
        process.exit(-1)
    })

const rabbit = require("rabbitmq-stream-js-client");

async function main() {
    const streamName = "test-queue-stream";

    console.log("Connecting...");
    const client = await rabbit.connect({
        vhost: "/",
        port: 5552,
        hostname: "localhost",
        username: "rabbit",
        password: "rabbit",
    });

    console.log("Making sure the stream exists...");
    await client.createStream({ stream: streamName, arguments: {} });

    console.log("Creating the publisher...");
    const publisher = await client.declarePublisher({ stream: streamName });

    console.log("Sending a message...");
    await publisher.send(Buffer.from("Test message"));

    console.log("Closing the connection...");
    await client.close();
}

main()
    .then(() => console.log("done!"))
    .catch((res) => {
        console.log("Error in publishing message!", res);
        process.exit(-1);
    });

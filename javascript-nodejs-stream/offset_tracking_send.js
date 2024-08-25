const rabbit = require("rabbitmq-stream-js-client");

async function main() {
  console.log("Connecting...");
  const client = await rabbit.connect({
    vhost: "/",
    port: 5552,
    hostname: "localhost",
    username: "guest",
    password: "guest",
  });

  console.log("Making sure the stream exists...");
  const streamName = "stream-offset-tracking-javascript";
  await client.createStream({ stream: streamName, arguments: {} });

  console.log("Creating the publisher...");
  const publisher = await client.declarePublisher({ stream: streamName });

  const messageCount = 100;
  console.log(`Publishing ${messageCount} messages`);
  for (let i = 0; i < messageCount; i++) {
    const body = i === messageCount - 1 ? "marker" : `hello ${i}`;
    await publisher.send(Buffer.from(body));
  }

  console.log("Closing the connection...");
  await client.close();
}

main()
  .then(() => console.log("done!"))
  .catch((res) => {
    console.log("Error in publishing message!", res);
    process.exit(-1);
  });

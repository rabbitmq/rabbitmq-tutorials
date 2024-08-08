const rabbit = require("rabbitmq-stream-js-client");

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function main() {
  const streamName = "stream-offset-tracking-javascript";

  console.log("Connecting...");
  const client = await rabbit.connect({
    hostname: "localhost",
    port: 5552,
    username: "guest",
    password: "guest",
    vhost: "/",
  });

  const consumerRef = "offset-tracking-tutorial";
  let firstOffset = undefined;
  let offsetSpecification = rabbit.Offset.first();
  try {
    const offset = await client.queryOffset({ reference: consumerRef, stream: streamName });
    firstOffset = offset + 1n;
    offsetSpecification = rabbit.Offset.offset(firstOffset);
  } catch (e) {}

  let lastOffset = offsetSpecification.value;
  let messageCount = 0;
  const consumer = await client.declareConsumer(
    { stream: streamName, offset: offsetSpecification, consumerRef },
    async (message) => {
      messageCount++;
      if (!firstOffset && messageCount === 1) {
        firstOffset = message.offset;
        console.log("First message received");
      }
      if (messageCount % 10 === 0) {
        console.log("Storing offset");
        await consumer.storeOffset(message.offset);
      }
      if (message.content.toString() === "marker") {
        console.log("Marker found");
        lastOffset = message.offset;
        await consumer.storeOffset(message.offset);
        console.log(`Done consuming, first offset was ${firstOffset}, last offset was ${lastOffset}`);
        await consumer.close(true);
      }
    }
  );

  console.log(`Start consuming...`);
  await sleep(2000);
}

main()
  .then(async () => {
    await new Promise(function () {});
  })
  .catch((res) => {
    console.log("Error while receiving message!", res);
    process.exit(-1);
  });

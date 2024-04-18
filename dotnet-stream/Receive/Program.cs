// See https://aka.ms/new-console-template for more information

using System.Text;
using RabbitMQ.Stream.Client;
using RabbitMQ.Stream.Client.Reliable;

var streamSystem = await StreamSystem.Create(new StreamSystemConfig());

await streamSystem.CreateStream(new StreamSpec("hello-stream")
{
    MaxLengthBytes = 5_000_000_000
});


var consumer = await Consumer.Create(new ConsumerConfig(streamSystem, "hello-stream")
{
    OffsetSpec = new OffsetTypeFirst(),
    MessageHandler = async (stream, _, _, message) =>
    {
        Console.WriteLine($"Stream: {stream} - " +
                          $"Received message: {Encoding.UTF8.GetString(message.Data.Contents)}");
        await Task.CompletedTask;
    }
});

Console.WriteLine(" [x] Press any key to exit");
Console.ReadKey();

await consumer.Close();
await streamSystem.Close();
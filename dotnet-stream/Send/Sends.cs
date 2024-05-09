using System.Text;
using RabbitMQ.Stream.Client;
using RabbitMQ.Stream.Client.Reliable;

var streamSystem = await StreamSystem.Create(new StreamSystemConfig());

await streamSystem.CreateStream(new StreamSpec("hello-stream")
{
    MaxLengthBytes = 5_000_000_000
});

var producer = await Producer.Create(new ProducerConfig(streamSystem, "hello-stream"));


await producer.Send(new Message(Encoding.UTF8.GetBytes($"Hello, World")));
Console.WriteLine(" [x] Sent 'Hello, World'");

Console.WriteLine(" [x] Press any key to exit");
Console.ReadKey();
await producer.Close();
await streamSystem.Close();
// See https://aka.ms/new-console-template for more information

using System.Text;
using RabbitMQ.Stream.Client;
using RabbitMQ.Stream.Client.Reliable;

var streamSystem = await StreamSystem.Create(new StreamSystemConfig());

await streamSystem.CreateStream(new StreamSpec("hello-stream")
{
    MaxLengthBytes = 5_000_000_000
});

var producer = await Producer.Create(new ProducerConfig(streamSystem, "hello-stream")
{
    ConfirmationHandler = async confirmation =>
    {
        switch (confirmation.Status)
        {
            case ConfirmationStatus.Confirmed:
                foreach (var message in confirmation.Messages)
                {
                    Console.WriteLine(
                        $"Stream: {confirmation.Stream}, " +
                        $"message: {Encoding.UTF8.GetString(message.Data.Contents)} confirmed");
                };
                break;
            default:
                foreach (var message in confirmation.Messages)
                {
                    Console.WriteLine(
                        $"Stream: {confirmation.Stream}, " +
                        $"message: {Encoding.UTF8.GetString(message.Data.Contents)} not confirmed " +
                        $"with status {confirmation.Status}");
                };
                break;
        }
        await Task.CompletedTask;
    }
});


for (var i = 0; i < 100; i++)
{
    await producer.Send(new Message(Encoding.UTF8.GetBytes($"Hello, World - {i}")));
}

await Task.Delay(500);

Console.WriteLine("Press any key to exit");
Console.ReadKey();
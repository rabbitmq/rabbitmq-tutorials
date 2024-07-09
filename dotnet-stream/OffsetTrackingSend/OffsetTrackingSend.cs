using System.Text;
using System.Threading.Tasks;
using RabbitMQ.Stream.Client;
using RabbitMQ.Stream.Client.Reliable;

var streamSystem = await StreamSystem.Create(new StreamSystemConfig());

var stream = "stream-offset-tracking-dotnet";
await streamSystem.CreateStream(new StreamSpec(stream));

var messageCount = 100;
var confirmedCde = new CountdownEvent(messageCount);
var producer = await Producer.Create(new ProducerConfig(streamSystem, stream) {
    ConfirmationHandler = async confirmation => {
        if (confirmation.Status == ConfirmationStatus.Confirmed) {
            confirmedCde.Signal();
        }
        await Task.CompletedTask.ConfigureAwait(false);
    }
});

Console.WriteLine("Publishing {0} messages...", messageCount);
for (int i = 0; i < messageCount; i++) {
    var body = i == messageCount - 1 ? "marker" : "hello";
    await producer.Send(new Message(Encoding.UTF8.GetBytes(body)));
}

confirmedCde.Wait();
Console.WriteLine("Messages confirmed.");
await producer.Close();
await streamSystem.Close();

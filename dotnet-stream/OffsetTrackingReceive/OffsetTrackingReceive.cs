using System.Text;
using System.Threading;
using System.Threading.Tasks;
using RabbitMQ.Stream.Client;
using RabbitMQ.Stream.Client.Reliable;

var streamSystem = await StreamSystem.Create(new StreamSystemConfig());

var stream = "stream-offset-tracking-dotnet";
await streamSystem.CreateStream(new StreamSpec(stream));

var consumerName = "offset-tracking-tutorial"; 
IOffsetType offsetSpecification;
try {
    ulong storedOffset = await streamSystem.QueryOffset(consumerName, stream).ConfigureAwait(false);
    offsetSpecification = new OffsetTypeOffset(storedOffset + 1);
} catch (OffsetNotFoundException) {
    offsetSpecification = new OffsetTypeFirst();
}
ulong initialValue = UInt64.MaxValue;
ulong firstOffset = initialValue;
int messageCount = 0;
ulong lastOffset = initialValue;
var consumedCde = new CountdownEvent(1);
var consumer = await Consumer.Create(new ConsumerConfig(streamSystem, stream)
{
    OffsetSpec = offsetSpecification,
    Reference =  consumerName,
    MessageHandler = async (_, consumer, context, message) => {
        if (Interlocked.CompareExchange(ref firstOffset, context.Offset, initialValue) == initialValue) {
            Console.WriteLine("First message received.");
        }
        if (Interlocked.Increment(ref messageCount) % 10 == 0) {
            await consumer.StoreOffset(context.Offset).ConfigureAwait(false);
        }
        if ("marker".Equals(Encoding.UTF8.GetString(message.Data.Contents))) {
            Interlocked.Exchange(ref lastOffset, context.Offset);
            await consumer.StoreOffset(context.Offset).ConfigureAwait(false);
            await consumer.Close();
            consumedCde.Signal();
        }
        await Task.CompletedTask;
    }
});
Console.WriteLine("Started consuming...");

consumedCde.Wait();
Console.WriteLine("Done consuming, first offset {0}, last offset {1}", firstOffset, lastOffset);
await streamSystem.Close();

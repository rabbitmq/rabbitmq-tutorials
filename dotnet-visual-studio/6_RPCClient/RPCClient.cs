using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System;
using System.Collections.Concurrent;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

class RpcClient
{
    private const string QUEUE_NAME = "rpc_queue";

    private readonly IConnection _connection;
    private readonly IModel _channel;
    private readonly string _replyQueueName;
    private readonly EventingBasicConsumer _consumer;
    private readonly ConcurrentDictionary<string, TaskCompletionSource<string>> _callbackMapper =
                new ConcurrentDictionary<string, TaskCompletionSource<string>>();

    public RpcClient()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };

        _connection = factory.CreateConnection();
        _channel = _connection.CreateModel();
        _replyQueueName = _channel.QueueDeclare().QueueName;
        _consumer = new EventingBasicConsumer(_channel);
        _consumer.Received += (model, ea) =>
        {
            if (!_callbackMapper.TryRemove(ea.BasicProperties.CorrelationId, out TaskCompletionSource<string> tcs))
                return;
            var body = ea.Body;
            var response = Encoding.UTF8.GetString(body);
            tcs.TrySetResult(response);
        };
    }

    public Task<string> CallAsync(string message, CancellationToken cancellationToken = default(CancellationToken))
    {
        IBasicProperties props = _channel.CreateBasicProperties();
        var correlationId = Guid.NewGuid().ToString();
        props.CorrelationId = correlationId;
        props.ReplyTo = _replyQueueName;
        var messageBytes = Encoding.UTF8.GetBytes(message);
        var tcs = new TaskCompletionSource<string>();
        _callbackMapper.TryAdd(correlationId, tcs);

        _channel.BasicPublish(
            exchange: "",
            routingKey: QUEUE_NAME,
            basicProperties: props,
            body: messageBytes);

        _channel.BasicConsume(
            consumer: _consumer,
            queue: _replyQueueName,
            autoAck: true);

        cancellationToken.Register(() => _callbackMapper.TryRemove(correlationId, out var tmp));
        return tcs.Task;
    }

    public void Close()
    {
        _connection.Close();
    }
}

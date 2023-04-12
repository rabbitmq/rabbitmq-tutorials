use futures::StreamExt;
use lapin::{
    options::*, types::FieldTable, types::ShortString, BasicProperties, Channel, Connection,
    ConnectionProperties, Consumer, Queue,
};
use std::convert::TryInto;
use std::fmt::Display;
use uuid::Uuid;

#[derive(Debug)]
enum Error {
    CannotDecodeReply,
    NoReply,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::CannotDecodeReply => write!(f, "Cannot decode reply"),
            Error::NoReply => write!(f, "No reply arrived"),
        }
    }
}

struct FibonacciRpcClient {
    conn: Connection,
    channel: Channel,
    callback_queue: Queue,
    consumer: Consumer,
    correlation_id: ShortString,
}

impl FibonacciRpcClient {
    async fn new() -> Result<Self, lapin::Error> {
        let addr = "amqp://127.0.0.1:5672";
        let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
        let channel = conn.create_channel().await?;
        let callback_queue = channel
            .queue_declare(
                "",
                QueueDeclareOptions {
                    exclusive: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await?;

        let consumer = channel
            .basic_consume(
                callback_queue.name().as_str(),
                "rpc_client",
                BasicConsumeOptions {
                    no_ack: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await?;

        let correlation_id = Uuid::new_v4().to_string().into();

        Ok(Self {
            conn,
            channel,
            callback_queue,
            consumer,
            correlation_id,
        })
    }

    async fn call(&mut self, n: u64) -> Result<u64, Box<dyn std::error::Error>> {
        self.channel
            .basic_publish(
                "",
                "rpc_queue",
                BasicPublishOptions::default(),
                &*n.to_le_bytes().to_vec(),
                BasicProperties::default()
                    .with_reply_to(self.callback_queue.name().clone())
                    .with_correlation_id(self.correlation_id.clone()),
            )
            .await?
            .await?;

        while let Some(delivery) = self.consumer.next().await {
            if let Ok(delivery) = delivery {
                if delivery.properties.correlation_id().as_ref() == Some(&self.correlation_id) {
                    return Ok(u64::from_le_bytes(
                        delivery
                            .data
                            .as_slice()
                            .try_into()
                            .map_err(|_| Error::CannotDecodeReply)?,
                    ));
                }
            }
        }

        Err(Box::new(Error::NoReply))
    }

    async fn close(&self) -> Result<(), lapin::Error> {
        self.conn.close(0, "").await
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut fibonacci_rpc = FibonacciRpcClient::new().await?;
    println!(" [x] Requesting fib(30)");
    let response = fibonacci_rpc.call(30).await?;
    println!(" [.] Got {}", response);
    fibonacci_rpc.close().await?;
    Ok(())
}

use std::convert::TryInto;
use std::fmt::Display;
use futures::StreamExt;
use lapin::{BasicProperties, Connection, ConnectionProperties, options::*, types::FieldTable};

#[derive(Debug)]
enum Error {
    CannotDecodeArg,
    MissingReplyTo,
    MissingCorrelationId,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::CannotDecodeArg => write!(f, "Cannot decode argument"),
            Error::MissingReplyTo => write!(f, "Missing 'reply to' property"),
            Error::MissingCorrelationId => write!(f, "Missing 'correlation id' property"),
        }
    }
}

fn fib(n: u64) -> u64 {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .queue_declare(
            "rpc_queue",
            QueueDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    channel.basic_qos(1, BasicQosOptions::default()).await?;

    let mut consumer = channel
        .basic_consume(
            "rpc_queue",
            "rpc_server",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await?;

    println!(" [x] Awaiting RPC requests");


    while let Some(delivery) = consumer.next().await {
        if let Ok(delivery) = delivery {
            println!(" [x] Received {:?}", std::str::from_utf8(&delivery.data)?);
            let n = u64::from_le_bytes(
                delivery
                    .data
                    .as_slice()
                    .try_into()
                    .map_err(|_| Error::CannotDecodeArg)?,
            );
            println!(" [.] fib({})", n);
            let response = fib(n);
            let payload = response.to_be_bytes();

            let routing_key = delivery
                .properties
                .reply_to()
                .as_ref()
                .ok_or(Error::MissingReplyTo)?
                .as_str();

            let correlation_id = delivery
                .properties
                .correlation_id()
                .clone()
                .ok_or(Error::MissingCorrelationId)?;

            channel
                .basic_publish(
                    "",
                    routing_key,
                    BasicPublishOptions::default(),
                    &payload,
                    BasicProperties::default().with_correlation_id(correlation_id),
                )
                .await?;

            channel
                .basic_ack(delivery.delivery_tag, BasicAckOptions::default())
                .await?;
        }
    }

    Ok(())
}

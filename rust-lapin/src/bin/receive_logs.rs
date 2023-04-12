use futures::StreamExt;
use lapin::{options::*, types::FieldTable, Connection, ConnectionProperties, ExchangeKind};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .exchange_declare(
            "logs",
            ExchangeKind::Fanout,
            ExchangeDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let queue = channel
        .queue_declare(
            "",
            QueueDeclareOptions {
                exclusive: true,
                ..Default::default()
            },
            FieldTable::default(),
        )
        .await?;

    channel
        .queue_bind(
            queue.name().as_str(),
            "logs",
            "",
            QueueBindOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let mut consumer = channel
        .basic_consume(
            queue.name().as_str(),
            "consumer",
            BasicConsumeOptions {
                no_ack: true,
                ..Default::default()
            },
            FieldTable::default(),
        )
        .await?;

    println!(" [*] Waiting for logs. To exit press CTRL+C");


    while let Some(delivery) = consumer.next().await {
        if let Ok(delivery) = delivery {
            println!(" [x] Received {:?}", std::str::from_utf8(&delivery.data)?);
        }
    }


    Ok(())
}

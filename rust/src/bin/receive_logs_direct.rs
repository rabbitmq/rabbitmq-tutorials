use futures::StreamExt;
use lapin::{Connection, ConnectionProperties, ExchangeKind, options::*, types::FieldTable};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let severities: Vec<_> = std::env::args().skip(1).collect();
    if severities.is_empty() {
        eprintln!(
            "Usage: {} [info] [warning] [error]\n",
            std::env::args().next().unwrap_or_else(|| "receive-direct".into())
        );
        std::process::exit(1);
    }

    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .exchange_declare(
            "direct_logs",
            ExchangeKind::Direct,
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


    futures::future::join_all(severities.iter().map(|severity| {
        channel.queue_bind(
            queue.name().as_str(),
            "direct_logs",
            &severity,
            QueueBindOptions::default(),
            FieldTable::default(),
        )
    })).await;

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
            println!(
                " [x] {}:{:?}",
                delivery.routing_key,
                std::str::from_utf8(&delivery.data)?
            );
        }
    }
    Ok(())
}

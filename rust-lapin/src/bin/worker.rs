use futures::StreamExt;
use std::thread;
use std::time::Duration;
use lapin::{Connection, ConnectionProperties, options::*, types::{AMQPValue, FieldTable}};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    let mut args = FieldTable::default();
    args.insert("x-queue-type".into(), AMQPValue::LongString("quorum".into()));
    channel
        .queue_declare(
            "task_queue",
            QueueDeclareOptions { durable: true, ..Default::default() },
            args,
        )
        .await?;

    let mut consumer = channel
        .basic_consume(
            "task_queue",
            "consumer",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await
        .expect("basic_consume");

    println!(" [*] Waiting for messages. To exit press CTRL+C");

    while let Some(delivery) = consumer.next().await {
        if let Ok(delivery) = delivery {
            println!(" [x] Received {:?}", std::str::from_utf8(&delivery.data)?);
            thread::sleep(Duration::from_secs(delivery.data.len() as u64));
            println!(" [x] Done");
            delivery.ack(BasicAckOptions::default())
                .await
                .expect("basic_ack");
        }
    }

    Ok(())
}

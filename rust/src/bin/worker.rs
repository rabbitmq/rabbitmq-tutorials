use lapin::{options::*, types::FieldTable, Connection, ConnectionProperties};
use std::thread;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .queue_declare(
            "task_queue",
            QueueDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let consumer = channel
        .basic_consume(
            "task_queue",
            "consumer",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await?;

    println!(" [*] Waiting for messages. To exit press CTRL+C");

    for delivery in consumer {
        let (channel, delivery) = delivery?;
        println!(" [x] Received {:?}", std::str::from_utf8(&delivery.data)?);
        thread::sleep(Duration::from_secs(delivery.data.len() as u64));
        println!(" [x] Done");
        channel
            .basic_ack(delivery.delivery_tag, BasicAckOptions::default())
            .await?;
    }

    Ok(())
}

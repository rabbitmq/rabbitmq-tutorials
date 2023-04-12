use futures::StreamExt;
use lapin::{Connection, ConnectionProperties, options::*, types::FieldTable};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .queue_declare(
            "hello",
            QueueDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let mut consumer = channel
        .basic_consume(
            "hello",
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
            delivery.ack(BasicAckOptions::default())
                .await
                .expect("basic_ack");
        }
    }

    Ok(())
}

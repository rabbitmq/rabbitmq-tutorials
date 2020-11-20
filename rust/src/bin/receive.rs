use lapin::{options::*, types::FieldTable, Connection, ConnectionProperties};

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

    let consumer = channel
        .basic_consume(
            "hello",
            "consumer",
            BasicConsumeOptions {
                no_ack: true,
                ..Default::default()
            },
            FieldTable::default(),
        )
        .await?;

    println!(" [*] Waiting for messages. To exit press CTRL+C");

    for delivery in consumer {
        let (_, delivery) = delivery?;
        println!(" [x] Received {:?}", std::str::from_utf8(&delivery.data)?);
    }

    Ok(())
}

use lapin::{options::*, types::{AMQPValue, FieldTable}, BasicProperties, Connection, ConnectionProperties};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    let mut args = FieldTable::default();
    args.insert("x-queue-type".into(), AMQPValue::LongString("quorum".into()));
    channel
        .queue_declare(
            "hello",
            QueueDeclareOptions { durable: true, ..Default::default() },
            args,
        )
        .await?;

    let payload = "Hello world!".as_bytes();
    channel
        .basic_publish(
            "",
            "hello",
            BasicPublishOptions::default(),
            payload,
            BasicProperties::default(),
        )
        .await?;

    println!(" [x] Sent \"Hello World!\"");

    conn.close(0, "").await?;

    Ok(())
}

use lapin::{options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties};

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

    channel
        .basic_publish(
            "",
            "hello",
            BasicPublishOptions::default(),
            b"Hello World!".to_vec(),
            BasicProperties::default(),
        )
        .await?;

    println!(" [x] Sent \"Hello World!\"");

    conn.close(0, "").await?;

    Ok(())
}

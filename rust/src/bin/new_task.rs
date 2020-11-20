use lapin::{options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let message = match args.len() {
        0 => b"hello".to_vec(),
        _ => args.join(" ").into_bytes(),
    };

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

    channel
        .basic_publish(
            "",
            "task_queue",
            BasicPublishOptions::default(),
            message.clone(),
            BasicProperties::default(),
        )
        .await?;

    println!(" [x] Sent {:?}", std::str::from_utf8(&message)?);

    conn.close(0, "").await?;

    Ok(())
}

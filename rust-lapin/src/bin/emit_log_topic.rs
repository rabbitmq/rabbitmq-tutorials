use lapin::{
    options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties, ExchangeKind,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let routing_key = args.first().map(String::as_str).unwrap_or("anonymous.info");
    let message = match args.len() {
        x if x < 2 => "Hello, world!".to_string(),
        _ => args[1..].join(" ").to_string(),
    };

    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .exchange_declare(
            "topic_logs",
            ExchangeKind::Topic,
            ExchangeDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    channel
        .basic_publish(
            "topic_logs",
            routing_key,
            BasicPublishOptions::default(),
            message.as_bytes(),
            BasicProperties::default(),
        )
        .await?;

    println!(
        " [x] Sent {}:{:?}",
        routing_key,
        std::str::from_utf8(message.as_bytes())?
    );

    conn.close(0, "").await?;

    Ok(())
}

use lapin::{
    options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties, ExchangeKind,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let severity = args.first().map(String::as_str).unwrap_or("info");
    let message = match args.len() {
        x if x < 2 => b"Hello, world!".to_vec(),
        _ => args[1..].join(" ").into_bytes(),
    };

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

    channel
        .basic_publish(
            "direct_logs",
            severity,
            BasicPublishOptions::default(),
            message.clone(),
            BasicProperties::default(),
        )
        .await?;

    println!(
        " [x] Sent {}:{:?}",
        severity,
        std::str::from_utf8(&message)?
    );

    conn.close(0, "").await?;

    Ok(())
}

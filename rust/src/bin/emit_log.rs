use lapin::{
    options::*, types::FieldTable, BasicProperties, Connection, ConnectionProperties, ExchangeKind,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let message = match args.len() {
        0 => "hello".to_string(),
        _ => args.join(" ").to_string(),
    };

    let addr = "amqp://127.0.0.1:5672";
    let conn = Connection::connect(addr, ConnectionProperties::default()).await?;
    let channel = conn.create_channel().await?;

    channel
        .exchange_declare(
            "logs",
            ExchangeKind::Fanout,
            ExchangeDeclareOptions::default(),
            FieldTable::default(),
        )
        .await?;

    channel
        .basic_publish(
            "logs",
            "",
            BasicPublishOptions::default(),
            message.as_bytes(),
            BasicProperties::default(),
        )
        .await?;

    println!(" [x] Sent {:?}", std::str::from_utf8(&message.as_bytes())?);

    conn.close(0, "").await?;

    Ok(())
}

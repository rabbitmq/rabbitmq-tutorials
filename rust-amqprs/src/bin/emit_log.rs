use amqprs::{
    connection::{Connection, OpenConnectionArguments},
    callbacks::{DefaultConnectionCallback, DefaultChannelCallback},
    channel::{ExchangeDeclareArguments, BasicPublishArguments}, BasicProperties
};
use tokio::{io::Error as TError};

#[tokio::main]
async fn main() -> Result<(), Box<TError>> {
    let conn = Connection::open(&OpenConnectionArguments::new(
        "localhost",
        5672,
        "guest",
        "guest")).await.unwrap();
    conn.register_callback(DefaultConnectionCallback).await.unwrap();

    let ch = conn.open_channel(None).await.unwrap();
    ch.register_callback(DefaultChannelCallback).await.unwrap();

    let x_name = "logs";
    let x_type = "fanout";
    let x_args = ExchangeDeclareArguments::new(x_name, x_type).durable(true).finish();
    ch.exchange_declare(x_args).await.unwrap();

    let args: Vec<_> = std::env::args().skip(1).collect();
    let payload = match args.len() {
        0 => "hello".to_string(),
        _ => args.join(" ").to_string(),
    };

    let publish_args = BasicPublishArguments::new(x_name, "");
    // publish messages as persistent
    let props = BasicProperties::default().with_delivery_mode(2).finish();
    ch.basic_publish(props, payload.clone().into_bytes(), publish_args).await.unwrap();

    println!(" [x] Sent {:?}", payload);

    ch.close().await.unwrap();
    conn.close().await.unwrap();

    Ok(())
}
use amqprs::{
    connection::{Connection, OpenConnectionArguments},
    callbacks::{DefaultConnectionCallback, DefaultChannelCallback},
    channel::{QueueDeclareArguments, BasicPublishArguments}, BasicProperties,
    FieldTable,
};
use amqp_serde::types::{FieldValue, ShortStr};
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

    let q_name = "task_queue";
    let mut args = FieldTable::new();
    args.insert(
        ShortStr::try_from("x-queue-type").unwrap(),
        FieldValue::S("quorum".try_into().unwrap()),
    );
    let q_args = QueueDeclareArguments::new(q_name).durable(true).arguments(args).finish();
    let (_, _, _) = ch.queue_declare(q_args).await.unwrap().unwrap();

    let args: Vec<_> = std::env::args().skip(1).collect();
    let payload = match args.len() {
        0 => "hello".to_string(),
        _ => args.join(" ").to_string(),
    };
    
    let publish_args = BasicPublishArguments::new("", &q_name);
    // publish messages as persistent
    let props = BasicProperties::default().with_delivery_mode(2).finish();
    ch.basic_publish(props, payload.clone().into_bytes(), publish_args).await.unwrap();

    println!(" [x] Sent {:?}", payload);
    
    ch.close().await.unwrap();
    conn.close().await.unwrap();

    Ok(())
}
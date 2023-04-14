use amqprs::{
    connection::{Connection, OpenConnectionArguments},
    callbacks::{DefaultConnectionCallback, DefaultChannelCallback}, channel::{QueueDeclareArguments, BasicPublishArguments}, BasicProperties
};
use tokio;
use tokio::io::Error as TError;

#[tokio::main]
async fn main() -> Result<(), Box<TError>> {
    let conn = Connection::open(&OpenConnectionArguments::new(
        "localhost",
        5672,
        "guest",
        "guest",
    ))
    .await.unwrap();
    conn.register_callback(DefaultConnectionCallback).await.unwrap();
    
    let ch = conn.open_channel(None).await.unwrap();
    ch.register_callback(DefaultChannelCallback).await.unwrap();
    
    let q_args = QueueDeclareArguments::default()
        .queue(String::from("hello"))
        .durable(true)
        .finish();
    let (queue_name, _, _) = ch.queue_declare(q_args).await.unwrap().unwrap();
    
    let payload = String::from("Hello world!").into_bytes();
    let publish_args = BasicPublishArguments::new("", &queue_name);
    // publish messages as persistent
    let props = BasicProperties::default().with_delivery_mode(2).finish();
    ch.basic_publish(props, payload, publish_args).await.unwrap();
    
    println!(" [x] Sent \"Hello World!\"");

    // in real applications connections are meant to be long lived
    conn.close().await.unwrap();
    
    Ok(())
}

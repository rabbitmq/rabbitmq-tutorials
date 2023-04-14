use amqprs::{
    connection::{Connection, OpenConnectionArguments},
    callbacks::{DefaultConnectionCallback, DefaultChannelCallback}, channel::{QueueDeclareArguments, BasicConsumeArguments}
};
use tokio::{self, sync::Notify};
use tokio::io::Error as TError;
use std::str;

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
    let consumer_args = BasicConsumeArguments::new(&queue_name, "receive.rs");
    let (_ctag, mut rx) = ch.basic_consume_rx(consumer_args).await.unwrap();
    
    tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            if let Some(payload) = msg.content {
                println!(" [x] Received {:?}", str::from_utf8(&payload).unwrap());
            }
        };
        
    });
    
    println!(" [*] Waiting for messages. To exit press CTRL+C");
    
    let guard = Notify::new();
    guard.notified().await;
    
    Ok(())
}

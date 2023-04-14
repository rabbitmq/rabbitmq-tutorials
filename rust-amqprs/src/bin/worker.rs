use amqprs::{
    connection::{Connection, OpenConnectionArguments},
    callbacks::{DefaultConnectionCallback, DefaultChannelCallback},
    channel::{QueueDeclareArguments, BasicConsumeArguments, BasicAckArguments}
};
use tokio::{io::Error as TError, sync::Notify};
use std::{time::Duration, thread};

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
    let q_args = QueueDeclareArguments::new(q_name).durable(true).finish();
    let (_, _, _) = ch.queue_declare(q_args).await.unwrap().unwrap();
    
    let consumer_args = BasicConsumeArguments::default().queue(String::from(q_name)).finish();
    let (_ctag, mut rx) = ch.basic_consume_rx(consumer_args).await.unwrap();
    
    println!(" [*] Waiting for messages. To exit press CTRL+C");
    
    tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            if let Some(payload) = msg.content {
                println!(" [x] Received {:?}", std::str::from_utf8(&payload).unwrap());
                thread::sleep(Duration::from_secs(payload.len() as u64));
                println!(" [x] Done");
                
                ch.basic_ack(BasicAckArguments::new(msg.deliver.unwrap().delivery_tag(), false)).await.unwrap();
            }
        }
    });
    
    let guard = Notify::new();
    guard.notified().await;
    
    Ok(())
}
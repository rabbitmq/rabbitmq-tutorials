use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, Message, ResponseCode};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use tokio::sync::Notify;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rabbitmq_stream_client::Environment;
    let environment = Environment::builder().build().await?;
    let stream = "stream-offset-tracking-rust";
    let message_count = 100;
    let confirmed_messages = Arc::new(AtomicU32::new(0));
    let notify_on_send = Arc::new(Notify::new());

    let create_response = environment
        .stream_creator()
        .max_length(ByteCapacity::GB(2))
        .create(stream)
        .await;

    if let Err(e) = create_response {
        if let StreamCreateError::Create { stream, status } = e {
            match status {
                // we can ignore this error because the stream already exists
                ResponseCode::StreamAlreadyExists => {}
                err => {
                    println!("Error creating stream: {:?} {:?}", stream, err);
                    std::process::exit(1);
                }
            }
        }
    }

    println!("Publishing {:?} messages", message_count);

    let producer = environment.producer().build(stream).await?;

    for i in 0..message_count {
        let msg;
        if i < message_count - 1 {
            msg = Message::builder().body(format!("hello{}", i)).build();
        } else {
            msg = Message::builder().body(format!("marker{}", i)).build();
        };

        let counter = confirmed_messages.clone();
        let notifier = notify_on_send.clone();
        producer
            .send(msg, move |_| {
                let inner_counter = counter.clone();
                let inner_notifier = notifier.clone();
                async move {
                    if inner_counter.fetch_add(1, Ordering::Relaxed) == message_count - 1 {
                        inner_notifier.notify_one();
                    }
                }
            })
            .await?;
    }

    notify_on_send.notified().await;
    println!("Messages confirmed: True");
    producer.close().await?;
    Ok(())
}

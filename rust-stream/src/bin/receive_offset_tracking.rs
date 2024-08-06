use futures::StreamExt;
use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, OffsetSpecification, ResponseCode};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use tokio::sync::Notify;
use tokio::task;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rabbitmq_stream_client::Environment;
    let environment = Environment::builder().build().await?;
    let stream = "pippo";
    let first_offset = Arc::new(AtomicI64::new(-1));
    let last_offset = Arc::new(AtomicI64::new(-1));
    let notify_on_close = Arc::new(Notify::new());
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

    let stored_offset:u64 = 45;
    let mut consumer = environment
        .consumer()
        .name("consumer-1")
        .offset(OffsetSpecification::Offset(stored_offset))
        .build(stream)
        .await
        .unwrap();

    println!("Started consuming");

    /*let mut stored_offset: u64 = consumer.query_offset().await.unwrap_or_else(|_| 0);

    if stored_offset > 0 {
        stored_offset += 1;
    }
    consumer = environment
        .consumer()
        .name("consumer-1")
        .offset(OffsetSpecification::Offset(42))
        .build(stream)
        .await
        .unwrap();*/

    let first_cloned_offset = first_offset.clone();
    let last_cloned_offset = last_offset.clone();
    let notify_on_close_cloned = notify_on_close.clone();

    task::spawn(async move {
        let mut received_messages = -1;
        while let Some(delivery) = consumer.next().await {
            let d = delivery.unwrap();

            println!("offset {} ", d.offset());
            if first_offset.load(Ordering::Relaxed) == -1 {
                println!("First message received");
                _ = first_offset.compare_exchange(
                    first_offset.load(Ordering::Relaxed),
                    d.offset() as i64,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                );
            }
            //received_messages = received_messages + 1;
            if String::from_utf8_lossy(d.message().data().unwrap()).contains("marker")
            {
                /*let _ = consumer
                    .store_offset(d.offset())
                    .await
                    .unwrap_or_else(|e| println!("Err: {}", e));*/
                if String::from_utf8_lossy(d.message().data().unwrap()).contains("marker") {
                    last_offset.store(d.offset() as i64, Ordering::Relaxed);
                    let handle = consumer.handle();
                    _ = handle.close().await;
                    notify_on_close_cloned.notify_one();
                    break;
                }
            }
        }
    });

    notify_on_close.notified().await;

    if first_cloned_offset.load(Ordering::Relaxed) != -1 {
        println!(
            "Done consuming first_offset: {:?} last_offset: {:?}  ",
            first_cloned_offset, last_cloned_offset
        );
    }

    Ok(())
}

use futures::StreamExt;
use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, OffsetSpecification, ResponseCode};
use std::io::stdin;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use tokio::task;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rabbitmq_stream_client::Environment;
    let environment = Environment::builder().build().await?;
    let stream = "stream-offset-tracking-rust";
    let received_messages = Arc::new(AtomicI64::new(-1));
    let first_offset = Arc::new(AtomicI64::new(-1));
    let last_offset = Arc::new(AtomicI64::new(-1));
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

    let mut consumer = environment
        .consumer()
        .name("consumer-1")
        .offset(OffsetSpecification::First)
        .build(stream)
        .await
        .unwrap();

    println!("Starting consuming");
    println!("Press any key to close the consumer");

    let mut stored_offset: u64 = consumer.query_offset().await.unwrap_or_else(|_| 0);

    if stored_offset >  0 {
        stored_offset += 1;
    }
    consumer = environment
        .consumer()
        .name("consumer-1")
        .offset(OffsetSpecification::Offset(stored_offset))
        .build(stream)
        .await
        .unwrap();

    let first_cloned_offset = first_offset.clone();
    let last_cloned_offset = last_offset.clone();

    task::spawn(async move {
        while let Some(delivery) = consumer.next().await {
            let d = delivery.unwrap();

            if first_offset.load(Ordering::Relaxed) == -1 {
                println!("consuming first message");
                _ = first_offset.compare_exchange(
                    first_offset.load(Ordering::Relaxed),
                    d.offset() as i64,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                );
            }

            if received_messages.fetch_add(1, Ordering::Relaxed) % 10 == 0
                || String::from_utf8_lossy(d.message().data().unwrap()).contains("marker")
            {
                let _ = consumer
                    .store_offset(d.offset())
                    .await
                    .unwrap_or_else(|e| println!("Err: {}", e));
                if String::from_utf8_lossy(d.message().data().unwrap()).contains("marker") {
                    last_offset.store(d.offset() as i64, Ordering::Relaxed);
                    let handle = consumer.handle();
                    _ = handle.close().await;

                }
            }
        }
    });

    _ = stdin().read_line(&mut "".to_string());

    if first_cloned_offset.load(Ordering::Relaxed) != -1 {
        println!(
            "Done consuming first_offset: {:?} last_offset: {:?}  ",
            first_cloned_offset, last_cloned_offset
        );
    }

    Ok(())
}

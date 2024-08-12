use futures::StreamExt;
use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, OffsetSpecification, ResponseCode};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rabbitmq_stream_client::Environment;
    let environment = Environment::builder().build().await?;
    let stream = "stream-offset-tracking-rust";
    let mut first_offset: i64 = -1;
    let mut last_offset: i64 = -1;
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

    println!("Started consuming");

    let mut stored_offset: u64 = consumer.query_offset().await.unwrap_or_else(|_| 0);

    if stored_offset > 0 {
        stored_offset += 1;
    }
    consumer = environment
        .consumer()
        .name("consumer-1")
        .offset(OffsetSpecification::Offset(stored_offset))
        .build(stream)
        .await
        .unwrap();

    let mut received_messages: i64 = -1;
    while let Some(delivery) = consumer.next().await {
        let d = delivery.unwrap();

        if first_offset == -1 {
            first_offset = d.offset() as i64;
        }
        received_messages = received_messages + 1;
        if received_messages % 10 == 0
            || String::from_utf8_lossy(d.message().data().unwrap()).contains("marker")
        {
            let _ = consumer
                .store_offset(d.offset())
                .await
                .unwrap_or_else(|e| println!("Err: {}", e));
            if String::from_utf8_lossy(d.message().data().unwrap()).contains("marker") {
                last_offset = d.offset() as i64;
                let handle = consumer.handle();
                _ = handle.close().await;
                break;
            }
        }
    }

    if first_offset != -1 {
        println!(
            "Done consuming first_offset: {:?} last_offset: {:?}  ",
            first_offset, last_offset
        );
    }

    Ok(())
}

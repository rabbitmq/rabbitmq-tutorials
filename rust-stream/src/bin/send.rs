use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, Message, ResponseCode};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rabbitmq_stream_client::Environment;
    let environment = Environment::builder().build().await?;
    let stream = "hello-rust-stream";
    let create_response = environment
        .stream_creator()
        .max_length(ByteCapacity::GB(5))
        .create(stream)
        .await;

    if let Err(e) = create_response {
        if let StreamCreateError::Create { stream, status } = e {
            match status {
                // we can ignore this error because the stream already exists
                ResponseCode::StreamAlreadyExists => {}
                err => {
                    println!("Error creating stream: {:?} {:?}", stream, err);
                }
            }
        }
    }

    let producer = environment.producer().build(stream).await?;

    producer
        .send_with_confirm(Message::builder().body("Hello, World!").build())
        .await?;
    println!("Sent message to stream: {}", stream);
    producer.close().await?;
    Ok(())
}

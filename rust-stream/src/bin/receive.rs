use std::io::stdin;
use rabbitmq_stream_client::error::StreamCreateError;
use rabbitmq_stream_client::types::{ByteCapacity, OffsetSpecification, ResponseCode};
use futures::{StreamExt};
use tokio::task;

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

    let mut consumer = environment
        .consumer()
        .offset(OffsetSpecification::First)
        .build(stream)
        .await
        .unwrap();

    let handle = consumer.handle();
    task::spawn(async move {
        while let Some(delivery) = consumer.next().await {
            let d = delivery.unwrap();
            println!("Got message: {:#?} with offset: {}",
                     d.message().data().map(|data| String::from_utf8(data.to_vec()).unwrap()),
                     d.offset(),);
        }
    });


    println!("Press any key to close the consumer");
     _ = stdin().read_line(&mut "".to_string());


    handle.close().await?;
    println!("consumer closed successfully");
    Ok(())
}

use crate::lapin::channel::{BasicProperties, BasicPublishOptions, QueueDeclareOptions};
use crate::lapin::client::ConnectionOptions;
use crate::lapin::types::FieldTable;
use failure::Error;
use futures::future::Future;
use itertools::free::join;
use lapin_futures as lapin;
use tokio;
use tokio::net::TcpStream;
use tokio::runtime::Runtime;

use lapin::client::Client as AMQPClient;

fn main() {
    let addr = "127.0.0.1:5672".parse().unwrap();
    let args: Vec<_> = std::env::args().skip(1).collect();
    let message = match args.len() {
        0 => "hello".to_string(),
        _ => join(args, " "),
    };

    Runtime::new()
        .unwrap()
        .block_on_all(
            TcpStream::connect(&addr) // try to initiate a TCP connection
                .map_err(Error::from)
                .and_then(|stream| {
                    // if successful, pass it to AMQP client
                    AMQPClient::connect(stream, ConnectionOptions::default()).map_err(Error::from)
                })
                .and_then(|(client, _)| client.create_channel().map_err(Error::from)) // create a channel
                .and_then(|channel| {
                    channel
                        // declare a new queue
                        .queue_declare("hello", QueueDeclareOptions::default(), FieldTable::new())
                        .and_then(move |_| {
                            // if successful, send a message
                            channel
                                .basic_publish(
                                    "",
                                    "hello",
                                    message.as_bytes().to_vec(),
                                    BasicPublishOptions::default(),
                                    BasicProperties::default(),
                                )
                                .map(|_| println!("Sent a message"))
                        })
                        .map_err(Error::from)
                }),
        )
        .expect("Failed to create tokio runtime");
}

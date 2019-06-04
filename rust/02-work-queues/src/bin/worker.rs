use crate::lapin::channel::{BasicConsumeOptions, QueueDeclareOptions};
use crate::lapin::client::ConnectionOptions;
use crate::lapin::types::FieldTable;
use failure::Error;
use futures::future::Future;
use futures::stream::Stream;
use lapin_futures as lapin;
use tokio;
use tokio::net::TcpStream;
use tokio::runtime::Runtime;
use std::thread;
use std::time::Duration;

use lapin::client::Client as AMQPClient;

fn main() {
    let addr = "127.0.0.1:5672".parse().unwrap();

    Runtime::new()
        .unwrap()
        .block_on_all(
            TcpStream::connect(&addr) // try to initiate a TCP connection
                .map_err(Error::from)
                .and_then(|stream| {
                    // if successful, pass it to AMQP client
                    AMQPClient::connect(stream, ConnectionOptions::default()).map_err(Error::from)
                })
                .and_then(|(client, heartbeat)| {
                    // do a heartbeat on a dedicated thread to keep us connected
                    tokio::spawn(heartbeat.map_err(|_| ()));
                    // create a channel
                    client.create_channel().map_err(Error::from)
                })
                .and_then(|channel| {
                    let ch = channel.clone();
                    channel
                        // declare a queue
                        .queue_declare("hello", QueueDeclareOptions::default(), FieldTable::new())
                        .and_then(move |queue| {
                            // create a message receiver
                            channel.basic_consume(
                                &queue,
                                "consumer",
                                BasicConsumeOptions::default(),
                                FieldTable::new(),
                            )
                        })
                        .and_then(|stream| {
                            // print received messages
                            stream.for_each(move |message| {
                                let text = std::str::from_utf8(&message.data).unwrap();
                                println!("Received: {:?}", text);
                                // Imitate a second of work per one symbol in message
                                thread::sleep(Duration::from_secs(text.len() as u64));
                                println!("Done");
                                ch.basic_ack(message.delivery_tag, false)
                            })
                        })
                        .map_err(Error::from)
                }),
        )
        .expect("Failed to create tokio runtime");
}

use crate::lapin::channel::{BasicProperties, BasicPublishOptions, ExchangeDeclareOptions};
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
    let severity = match args.len() {
        0 => "info".to_string(),
        _ => args.first().unwrap().clone(),
    };
    let message = match args.len() {
        x if x < 2 => "Hello, world!".to_string(),
        _ => join(args[1..].iter(), " "),
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
                .and_then(move |c| {
                    let channel = c.clone();
                    channel
                        // declare a new exchange
                        .exchange_declare(
                            "direct_logs",
                            "direct",
                            ExchangeDeclareOptions::default(),
                            FieldTable::new(),
                        )
                        .map(move |_| channel.clone())
                        .and_then(move |ch| {
                            // if successful, send a message
                            ch.basic_publish(
                                "direct_logs",
                                &severity,
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

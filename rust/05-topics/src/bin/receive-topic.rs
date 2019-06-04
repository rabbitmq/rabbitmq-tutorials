use crate::lapin::channel::{
    BasicConsumeOptions, ExchangeDeclareOptions, QueueBindOptions, QueueDeclareOptions,
};
use crate::lapin::client::ConnectionOptions;
use crate::lapin::types::FieldTable;
use failure::Error;
use futures::future::Future;
use futures::stream::Stream;
use lapin_futures as lapin;
use tokio;
use tokio::net::TcpStream;
use tokio::runtime::Runtime;

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
                .and_then(|c| {
                    let channel = c.clone();
                    // generate an queue with a random name which deletes itself after use
                    let queue_options = QueueDeclareOptions {
                        durable: false,
                        exclusive: true,
                        auto_delete: true,
                        nowait: false,
                        passive: false,
                        ticket: 0u16,
                    };
                    channel
                        .exchange_declare(
                            "topic_logs",
                            "topic",
                            ExchangeDeclareOptions::default(),
                            FieldTable::new(),
                        )
                        .map(move |_| channel.clone())
                        // declare a queue
                        .and_then(move |ch| {
                            // declare a queue using specified options
                            // if name is empty it will be generated
                            ch.queue_declare("", queue_options, FieldTable::new())
                                .map(move |queue| (ch.clone(), queue))
                        })
                        .and_then(move |(ch, queue)| {
                            // bind our queue to declared exchange
                            let name = queue.name();
                            let c = ch.clone();
                            let args: Vec<_> = std::env::args().skip(1).collect();
                            let topics = match args.len() {
                                0 => vec!["anonymous.info".to_string()],
                                _ => args,
                            };
                            let binds = topics.into_iter().map(move |topic| {
                                c.queue_bind(
                                    &name,
                                    "topic_logs",
                                    &topic,
                                    QueueBindOptions::default(),
                                    FieldTable::new(),
                                )
                            });
                            futures::future::join_all(binds).map(move |_| (ch.clone(), queue))
                        })
                        .and_then(move |(ch, queue)| {
                            // create a message receiver
                            ch.basic_consume(
                                &queue,
                                "consumer",
                                BasicConsumeOptions::default(),
                                FieldTable::new(),
                            )
                            .map(move |s| (ch.clone(), s))
                        })
                        .and_then(move |(ch, stream)| {
                            // print received messages
                            stream.for_each(move |message| {
                                let topic = &message.routing_key;
                                let text = std::str::from_utf8(&message.data).unwrap();
                                println!("Received: [{:?}] {:?}", topic, text);
                                ch.basic_ack(message.delivery_tag, false)
                            })
                        })
                        .map_err(Error::from)
                }),
        )
        .expect("Failed to create tokio runtime");
}

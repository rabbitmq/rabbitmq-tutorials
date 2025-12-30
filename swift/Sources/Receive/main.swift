//
// RabbitMQ Swift Tutorials
// Tutorial 1: Hello World - Receive
//
// https://www.rabbitmq.com/tutorials/tutorial-one-swift
//

import BunnySwift

@main
struct Receive {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let queue = try await channel.queue("hello")

    print(" [*] Waiting for messages. To exit press CTRL+C")

    let consumer = try await channel.basicConsume(
      queue: queue.name,
      acknowledgementMode: .automatic
    )
    for try await message in consumer {
      print(" [x] Received '\(message.bodyString ?? "")'")
    }
  }
}

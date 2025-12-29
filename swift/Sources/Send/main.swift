//
// RabbitMQ Swift Tutorials
// Tutorial 1: Hello World - Send
//
// https://www.rabbitmq.com/tutorials/tutorial-one-swift
//

import BunnySwift
import Foundation

@main
struct Send {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let queue = try await channel.queue("hello")

    try await channel.basicPublish(
      body: Data("Hello World!".utf8),
      routingKey: queue.name
    )
    print(" [x] Sent 'Hello World!'")

    try await connection.close()
  }
}

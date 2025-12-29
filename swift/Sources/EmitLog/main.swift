//
// RabbitMQ Swift Tutorials
// Tutorial 3: Publish/Subscribe - EmitLog
//
// https://www.rabbitmq.com/tutorials/tutorial-three-swift
//

import BunnySwift
import Foundation

@main
struct EmitLog {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.fanout("logs")

    let args = CommandLine.arguments.dropFirst()
    let message = args.isEmpty ? "Hello World!" : args.joined(separator: " ")

    try await channel.basicPublish(
      body: Data(message.utf8),
      exchange: exchange.name,
      routingKey: ""
    )
    print(" [x] Sent '\(message)'")

    try await connection.close()
  }
}

//
// RabbitMQ Swift Tutorials
// Tutorial 5: Topics - EmitLogTopic
//
// https://www.rabbitmq.com/tutorials/tutorial-five-swift
//

import BunnySwift
import Foundation

@main
struct EmitLogTopic {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.topic("topic_logs")

    var args = Array(CommandLine.arguments.dropFirst())
    let routingKey = args.isEmpty ? "anonymous.info" : args.removeFirst()
    let message = args.isEmpty ? "Hello World!" : args.joined(separator: " ")

    try await channel.basicPublish(
      body: Data(message.utf8),
      exchange: exchange.name,
      routingKey: routingKey
    )
    print(" [x] Sent '\(routingKey):\(message)'")

    try await connection.close()
  }
}

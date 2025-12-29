//
// RabbitMQ Swift Tutorials
// Tutorial 4: Routing - EmitLogDirect
//
// https://www.rabbitmq.com/tutorials/tutorial-four-swift
//

import BunnySwift
import Foundation

@main
struct EmitLogDirect {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.direct("direct_logs")

    var args = Array(CommandLine.arguments.dropFirst())
    let severity = args.isEmpty ? "info" : args.removeFirst()
    let message = args.isEmpty ? "Hello World!" : args.joined(separator: " ")

    try await channel.basicPublish(
      body: Data(message.utf8),
      exchange: exchange.name,
      routingKey: severity
    )
    print(" [x] Sent '\(severity):\(message)'")

    try await connection.close()
  }
}

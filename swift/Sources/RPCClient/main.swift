//
// RabbitMQ Swift Tutorials
// Tutorial 6: RPC - RPCClient
//
// https://www.rabbitmq.com/tutorials/tutorial-six-swift
//

import BunnySwift
import Foundation

@main
struct RPCClient {
  static func main() async throws {
    let n = Int(CommandLine.arguments.dropFirst().first ?? "30") ?? 30

    let connection = try await Connection.open()
    let channel = try await connection.openChannel()

    let replyQueue = try await channel.queue("", exclusive: true)
    let correlationId = UUID().uuidString

    print(" [x] Requesting fib(\(n))")

    try await channel.basicPublish(
      body: Data("\(n)".utf8),
      routingKey: "rpc_queue",
      properties: BasicProperties(
        correlationId: correlationId,
        replyTo: replyQueue.name
      )
    )

    let consumer = try await channel.basicConsume(
      queue: replyQueue.name,
      acknowledgementMode: .automatic
    )
    for try await message in consumer {
      if message.properties.correlationId == correlationId {
        let result = message.bodyString ?? "?"
        print(" [.] Got \(result)")
        break
      }
    }

    try await connection.close()
  }
}

//
// RabbitMQ Swift Tutorials
// Tutorial 2: Work Queues - NewTask
//
// https://www.rabbitmq.com/tutorials/tutorial-two-swift
//

import BunnySwift
import Foundation

@main
struct NewTask {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let queue = try await channel.queue("task_queue", durable: true)

    let args = CommandLine.arguments.dropFirst()
    let message = args.isEmpty ? "Hello World!" : args.joined(separator: " ")

    try await channel.basicPublish(
      body: Data(message.utf8),
      routingKey: queue.name,
      properties: .persistent
    )
    print(" [x] Sent '\(message)'")

    try await connection.close()
  }
}

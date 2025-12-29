//
// RabbitMQ Swift Tutorials
// Tutorial 3: Publish/Subscribe - ReceiveLogs
//
// https://www.rabbitmq.com/tutorials/tutorial-three-swift
//

import BunnySwift

@main
struct ReceiveLogs {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.fanout("logs")
    let queue = try await channel.queue("", exclusive: true)

    try await queue.bind(to: exchange)

    print(" [*] Waiting for logs. To exit press CTRL+C")

    let stream = try await channel.basicConsume(
      queue: queue.name,
      acknowledgementMode: .automatic
    )
    for try await message in stream {
      print(" [x] \(message.bodyString ?? "")")
    }
  }
}

//
// RabbitMQ Swift Tutorials
// Tutorial 4: Routing - ReceiveLogsDirect
//
// https://www.rabbitmq.com/tutorials/tutorial-four-swift
//

import BunnySwift

@main
struct ReceiveLogsDirect {
  static func main() async throws {
    let severities = Array(CommandLine.arguments.dropFirst())
    guard !severities.isEmpty else {
      print("Usage: ReceiveLogsDirect [info] [warning] [error]")
      return
    }

    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.direct("direct_logs")
    let queue = try await channel.queue("", exclusive: true)

    for severity in severities {
      try await queue.bind(to: exchange, routingKey: severity)
    }

    print(" [*] Waiting for logs. To exit press CTRL+C")

    let consumer = try await channel.basicConsume(
      queue: queue.name,
      acknowledgementMode: .automatic
    )
    for try await message in consumer {
      print(" [x] \(message.deliveryInfo.routingKey):\(message.bodyString ?? "")")
    }
  }
}

//
// RabbitMQ Swift Tutorials
// Tutorial 5: Topics - ReceiveLogsTopic
//
// https://www.rabbitmq.com/tutorials/tutorial-five-swift
//

import BunnySwift

@main
struct ReceiveLogsTopic {
  static func main() async throws {
    let bindingKeys = Array(CommandLine.arguments.dropFirst())
    guard !bindingKeys.isEmpty else {
      print("Usage: ReceiveLogsTopic [binding_key]...")
      return
    }

    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let exchange = try await channel.topic("topic_logs")
    let queue = try await channel.queue("", exclusive: true)

    for key in bindingKeys {
      try await queue.bind(to: exchange, routingKey: key)
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

//
// RabbitMQ Swift Tutorials
// Tutorial 2: Work Queues - Worker
//
// https://www.rabbitmq.com/tutorials/tutorial-two-swift
//

import BunnySwift

@main
struct Worker {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()
    let queue = try await channel.queue("task_queue", durable: true)

    try await channel.basicQos(prefetchCount: 1)
    print(" [*] Waiting for messages. To exit press CTRL+C")

    let consumer = try await channel.basicConsume(queue: queue.name)
    for try await message in consumer {
      let body = message.bodyString ?? ""
      print(" [x] Received '\(body)'")

      // Simulate work: count the dots in the message
      let dots = body.filter { $0 == "." }.count
      try await Task.sleep(for: .seconds(dots))

      print(" [x] Done")
      try await message.ack()
    }
  }
}

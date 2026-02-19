//
// RabbitMQ Swift Tutorials
// Tutorial 6: RPC - RPCServer
//
// https://www.rabbitmq.com/tutorials/tutorial-six-swift
//

import BunnySwift
import Foundation

func fibonacci(_ n: Int) -> Int {
  if n <= 1 { return n }
  return fibonacci(n - 1) + fibonacci(n - 2)
}

@main
struct RPCServer {
  static func main() async throws {
    let connection = try await Connection.open()
    let channel = try await connection.openChannel()

    let queue = try await channel.queue("rpc_queue")
    try await channel.basicQos(prefetchCount: 1)

    print(" [x] Awaiting RPC requests")

    let consumer = try await channel.basicConsume(queue: queue.name)
    for try await message in consumer {
      let n = Int(message.bodyString ?? "0") ?? 0
      print(" [.] fib(\(n))")

      let result = fibonacci(n)

      if let replyTo = message.properties.replyTo {
        try await channel.basicPublish(
          body: Data("\(result)".utf8),
          routingKey: replyTo,
          properties: BasicProperties(correlationId: message.properties.correlationId)
        )
      }
      try await message.ack()
    }
  }
}

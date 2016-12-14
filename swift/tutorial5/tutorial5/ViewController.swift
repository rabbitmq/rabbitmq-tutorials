//
//  ViewController.swift
//  tutorial5
//
//  Copyright © 2016 RabbitMQ. All rights reserved.
//
import UIKit
import RMQClient

class ViewController: UIViewController {


    override func viewDidLoad() {
        super.viewDidLoad()
        self.receiveLogsTopic(["kern.*", "*.critical"])
        sleep(2)
        self.emitLogTopic("Hello World!", routingKey: "kern.info")
        self.emitLogTopic("A critical kernel error", routingKey: "kern.critical")
        self.emitLogTopic("Critical module error", routingKey: "somemod.critical")
        self.emitLogTopic("Just some module info. You won't get this.", routingKey: "somemod.info")
    }

    func receiveLogsTopic(_ routingKeys: [String]) {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.topic("topic_logs")
        let q = ch.queue("", options: .exclusive)
        for routingKey: String in routingKeys {
            q.bind(x, routingKey: routingKey)
        }
        print("Waiting for logs.")
        q.subscribe({(_ message: RMQMessage) -> Void in
            print("\(message.routingKey!):\(String(data: message.body, encoding: .utf8)!)")
        })
    }

    func emitLogTopic(_ msg: String, routingKey: String) {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.topic("topic_logs")
        x.publish(msg.data(using: .utf8), routingKey: routingKey)
        print("Sent '\(msg)'")
        conn.close()
    }
}

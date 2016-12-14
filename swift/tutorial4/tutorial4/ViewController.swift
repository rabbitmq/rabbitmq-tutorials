//
//  ViewController.swift
//  tutorial4
//
//  Copyright © 2016 RabbitMQ. All rights reserved.
//
import UIKit
import RMQClient

class ViewController: UIViewController {


    override func viewDidLoad() {
        super.viewDidLoad()
        self.receiveLogsDirect()
        sleep(2)
        self.emitLogDirect("Hello World!", severity: "info")
        self.emitLogDirect("Missile button pressed", severity: "warning")
        self.emitLogDirect("Launch mechanism jammed", severity: "error")
    }

    func receiveLogsDirect() {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.direct("direct_logs")
        let q = ch.queue("", options: .exclusive)
        let severities = ["error", "warning", "info"]
        for severity: String in severities {
            q.bind(x, routingKey: severity)
        }
        print("Waiting for logs.")
        q.subscribe({(_ message: RMQMessage) -> Void in
            print("\(message.routingKey!):\(String(data: message.body, encoding: .utf8)!)")
        })
    }

    func emitLogDirect(_ msg: String, severity: String) {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.direct("direct_logs")
        x.publish(msg.data(using: .utf8), routingKey: severity)
        print("Sent '\(msg)'")
        conn.close()
    }
}

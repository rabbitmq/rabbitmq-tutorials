//
//  ViewController.swift
//  tutorial3
//
//  Copyright © 2016 RabbitMQ. All rights reserved.
//
import UIKit

import RMQClient

class ViewController: UIViewController {


    override func viewDidLoad() {
        super.viewDidLoad()
        self.receiveLogs()
        self.receiveLogs()
        sleep(1)
        self.emitLog()
    }

    func emitLog() {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.fanout("logs")
        let msg = "Hello World!"
        x.publish(msg.data(using: String.Encoding.utf8))
        print("Sent \(msg)")
        conn.close()
    }

    func receiveLogs() {
        let conn = RMQConnection(delegate: RMQConnectionDelegateLogger())
        conn.start()
        let ch = conn.createChannel()
        let x = ch.fanout("logs")
        let q = ch.queue("", options: .exclusive)
        q.bind(x)
        print("Waiting for logs.")
        q.subscribe({(_ message: RMQMessage) -> Void in
            print("Received \(String(data: message.body, encoding: String.Encoding.utf8)!)")
        })
    }
}

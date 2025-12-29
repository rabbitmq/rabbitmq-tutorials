// swift-tools-version: 6.0
//
// RabbitMQ Swift Tutorials
// https://www.rabbitmq.com/tutorials
//
// Copyright (c) 2016-2026 Broadcom. All Rights Reserved.
// The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
//
// Licensed under the Apache License, version 2.0 (the "License");
// you may not use these files except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import PackageDescription

let package = Package(
  name: "RabbitMQTutorials",
  platforms: [.macOS(.v14), .iOS(.v17), .tvOS(.v17), .watchOS(.v10), .visionOS(.v1)],
  dependencies: [
    // Use local path for development; switch to GitHub URL for release:
    // .package(url: "https://github.com/michaelklishin/bunny-swift.git", branch: "main"),
    .package(path: "../../bunny-swift.git")
  ],
  targets: [
    // Tutorial 1: Hello World
    .executableTarget(
      name: "Send", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),
    .executableTarget(
      name: "Receive", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),

    // Tutorial 2: Work Queues
    .executableTarget(
      name: "NewTask", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),
    .executableTarget(
      name: "Worker", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),

    // Tutorial 3: Publish/Subscribe
    .executableTarget(
      name: "EmitLog", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),
    .executableTarget(
      name: "ReceiveLogs", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),

    // Tutorial 4: Routing
    .executableTarget(
      name: "EmitLogDirect", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),
    .executableTarget(
      name: "ReceiveLogsDirect",
      dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),

    // Tutorial 5: Topics
    .executableTarget(
      name: "EmitLogTopic", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]),
    .executableTarget(
      name: "ReceiveLogsTopic", dependencies: [.product(name: "BunnySwift", package: "bunny-swift")]
    ),
  ]
)

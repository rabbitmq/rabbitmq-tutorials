# Swift code for RabbitMQ tutorials

Swift code examples for the [RabbitMQ tutorials](https://www.rabbitmq.com/tutorials).

These tutorials use [BunnySwift](https://github.com/michaelklishin/bunny-swift), a modern Swift 6 RabbitMQ client with async/await support.

## Requirements

- Swift 6.0 or later
- macOS 14+ (or iOS 17+, tvOS 17+, watchOS 10+, visionOS 1+)
- A running RabbitMQ server on localhost

## Building

Build all tutorials:

```sh
swift build
```

## Running the Tutorials

### Tutorial 1: Hello World

In one terminal, start the receiver:

```sh
swift run Receive
```

In another terminal, send a message:

```sh
swift run Send
```

### Tutorial 2: Work Queues

Start one or more workers:

```sh
swift run Worker
```

Send tasks with varying workloads (dots indicate seconds of work):

```sh
swift run NewTask "A simple task."
swift run NewTask "A longer task..."
swift run NewTask "A very long task....."
```

### Tutorial 3: Publish/Subscribe

Start one or more log receivers:

```sh
swift run ReceiveLogs
```

Emit log messages:

```sh
swift run EmitLog "Hello subscribers!"
```

### Tutorial 4: Routing

Subscribe to specific severity levels:

```sh
swift run ReceiveLogsDirect error warning
```

Emit logs with severity:

```sh
swift run EmitLogDirect info "Just an info message"
swift run EmitLogDirect warning "This is a warning"
swift run EmitLogDirect error "This is an error!"
```

### Tutorial 5: Topics

Subscribe using topic patterns (`*` matches one word, `#` matches zero or more):

```sh
swift run ReceiveLogsTopic "kern.*"
swift run ReceiveLogsTopic "*.critical"
swift run ReceiveLogsTopic "#"
```

Emit logs with topic routing keys:

```sh
swift run EmitLogTopic "kern.info" "Kernel info"
swift run EmitLogTopic "kern.critical" "Kernel critical error"
swift run EmitLogTopic "auth.critical" "Authentication failure"
```

## Tutorial Source Files

| Tutorial | Producer | Consumer |
|----------|----------|----------|
| 1. Hello World | [Send](Sources/Send/main.swift) | [Receive](Sources/Receive/main.swift) |
| 2. Work Queues | [NewTask](Sources/NewTask/main.swift) | [Worker](Sources/Worker/main.swift) |
| 3. Pub/Sub | [EmitLog](Sources/EmitLog/main.swift) | [ReceiveLogs](Sources/ReceiveLogs/main.swift) |
| 4. Routing | [EmitLogDirect](Sources/EmitLogDirect/main.swift) | [ReceiveLogsDirect](Sources/ReceiveLogsDirect/main.swift) |
| 5. Topics | [EmitLogTopic](Sources/EmitLogTopic/main.swift) | [ReceiveLogsTopic](Sources/ReceiveLogsTopic/main.swift) |

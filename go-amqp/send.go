package main

import (
	"context"
	"log"

	rmq "github.com/rabbitmq/rabbitmq-amqp-go-client/pkg/rabbitmqamqp"
)

const brokerURI = "amqp://guest:guest@localhost:5672/"

func main() {
	ctx := context.Background()
	env := rmq.NewEnvironment(brokerURI, nil)
	conn, err := env.NewConnection(ctx)
	if err != nil {
		log.Panicf("Failed to connect to RabbitMQ: %v", err)
	}
	defer func() {
		_ = env.CloseConnections(context.Background())
	}()

	_, err = conn.Management().DeclareQueue(ctx, &rmq.QuorumQueueSpecification{Name: "hello"})
	if err != nil {
		log.Panicf("Failed to declare a queue: %v", err)
	}

	publisher, err := conn.NewPublisher(ctx, &rmq.QueueAddress{Queue: "hello"}, nil)
	if err != nil {
		log.Panicf("Failed to create publisher: %v", err)
	}
	defer func() { _ = publisher.Close(context.Background()) }()

	body := "Hello World!"
	res, err := publisher.Publish(ctx, rmq.NewMessage([]byte(body)))
	if err != nil {
		log.Panicf("Failed to publish a message: %v", err)
	}
	switch res.Outcome.(type) {
	case *rmq.StateAccepted:
	default:
		log.Panicf("Unexpected publish outcome: %v", res.Outcome)
	}
	log.Printf(" [x] Sent %s\n", body)
}

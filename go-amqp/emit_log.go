package main

import (
	"context"
	"log"
	"os"
	"strings"

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

	_, err = conn.Management().DeclareExchange(ctx, &rmq.FanOutExchangeSpecification{Name: "logs"})
	if err != nil {
		log.Panicf("Failed to declare an exchange: %v", err)
	}

	publisher, err := conn.NewPublisher(ctx, &rmq.ExchangeAddress{Exchange: "logs", Key: ""}, nil)
	if err != nil {
		log.Panicf("Failed to create publisher: %v", err)
	}
	defer func() { _ = publisher.Close(context.Background()) }()

	body := bodyFrom(os.Args)
	res, err := publisher.Publish(ctx, rmq.NewMessage([]byte(body)))
	if err != nil {
		log.Panicf("Failed to publish a message: %v", err)
	}
	switch res.Outcome.(type) {
	case *rmq.StateAccepted:
	default:
		log.Panicf("Unexpected publish outcome: %v", res.Outcome)
	}
	log.Printf(" [x] Sent %s", body)
}

func bodyFrom(args []string) string {
	var s string
	if (len(args) < 2) || args[1] == "" {
		s = "hello"
	} else {
		s = strings.Join(args[1:], " ")
	}
	return s
}

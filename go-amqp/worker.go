package main

import (
	"bytes"
	"context"
	"errors"
	"log"
	"time"

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

	_, err = conn.Management().DeclareQueue(ctx, &rmq.QuorumQueueSpecification{Name: "task_queue"})
	if err != nil {
		log.Panicf("Failed to declare a queue: %v", err)
	}

	consumer, err := conn.NewConsumer(ctx, "task_queue", &rmq.ConsumerOptions{InitialCredits: 1})
	if err != nil {
		log.Panicf("Failed to create consumer: %v", err)
	}
	defer func() { _ = consumer.Close(context.Background()) }()

	log.Printf(" [*] Waiting for messages. To exit press CTRL+C")
	for {
		delivery, err := consumer.Receive(ctx)
		if err != nil {
			if errors.Is(err, context.Canceled) {
				return
			}
			log.Panicf("Failed to receive a message: %v", err)
		}
		msg := delivery.Message()
		var payload []byte
		if len(msg.Data) > 0 {
			payload = msg.Data[0]
		}
		log.Printf("Received a message: %s", payload)
		dotCount := bytes.Count(payload, []byte("."))
		t := time.Duration(dotCount)
		time.Sleep(t * time.Second)
		log.Printf("Done")
		err = delivery.Accept(ctx)
		if err != nil {
			log.Panicf("Failed to accept message: %v", err)
		}
	}
}

package main

import (
	"context"
	"log"
	"strconv"

	amqp "github.com/Azure/go-amqp"
	rmq "github.com/rabbitmq/rabbitmq-amqp-go-client/pkg/rabbitmqamqp"
)

const brokerURI = "amqp://guest:guest@localhost:5672/"

func fib(n int) int {
	if n == 0 {
		return 0
	} else if n == 1 {
		return 1
	}
	return fib(n-1) + fib(n-2)
}

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

	_, err = conn.Management().DeclareQueue(ctx, &rmq.QuorumQueueSpecification{Name: "rpc_queue"})
	if err != nil {
		log.Panicf("Failed to declare a queue: %v", err)
	}
	_, err = conn.Management().PurgeQueue(ctx, "rpc_queue")
	if err != nil {
		log.Printf("purge: %v", err)
	}

	responder, err := conn.NewResponder(ctx, rmq.ResponderOptions{
		RequestQueue: "rpc_queue",
		Handler: func(_ context.Context, request *amqp.Message) (*amqp.Message, error) {
			var payload []byte
			if len(request.Data) > 0 {
				payload = request.Data[0]
			}
			n, err := strconv.Atoi(string(payload))
			if err != nil {
				return nil, err
			}
			log.Printf(" [.] fib(%d)", n)
			response := fib(n)
			return amqp.NewMessage([]byte(strconv.Itoa(response))), nil
		},
	})
	if err != nil {
		log.Panicf("Failed to create responder: %v", err)
	}
	defer func() { _ = responder.Close(context.Background()) }()

	log.Printf(" [*] Awaiting RPC requests")
	select {}
}

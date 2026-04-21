package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	amqp "github.com/Azure/go-amqp"
	rmq "github.com/rabbitmq/rabbitmq-amqp-go-client/pkg/rabbitmqamqp"
)

const (
	amqpURL      = "amqp://guest:guest@localhost:5672/"
	requestQueue = "rpc-requests"
)

func rpcServer(ctx context.Context) error {
	env := rmq.NewEnvironment(amqpURL, nil)
	conn, err := env.NewConnection(ctx)
	if err != nil {
		return fmt.Errorf("failed to connect to RabbitMQ: %w", err)
	}
	defer func() { _ = env.CloseConnections(context.Background()) }()

	_, err = conn.Management().DeclareQueue(ctx, &rmq.QuorumQueueSpecification{Name: requestQueue})
	if err != nil {
		return fmt.Errorf("failed to declare queue: %w", err)
	}

	responder, err := conn.NewResponder(ctx, rmq.ResponderOptions{
		RequestQueue: requestQueue,
		Handler: func(_ context.Context, request *amqp.Message) (*amqp.Message, error) {
			var payload string
			if len(request.Data) > 0 {
				payload = string(request.Data[0])
			}
			if request.Properties != nil && request.Properties.MessageID != nil {
				log.Printf("RPC Server: Received %s request (ID: %v)", payload, request.Properties.MessageID)
			} else {
				log.Printf("RPC Server: Received %s request", payload)
			}
			return amqp.NewMessage([]byte("pong")), nil
		},
	})
	if err != nil {
		return fmt.Errorf("failed to create responder: %w", err)
	}
	defer func() { _ = responder.Close(context.Background()) }()

	log.Println("RPC Server: Started and listening for requests...")

	<-ctx.Done()
	log.Println("RPC Server: Shutting down...")
	return nil
}

func rpcClient(ctx context.Context) error {
	env := rmq.NewEnvironment(amqpURL, nil)
	conn, err := env.NewConnection(ctx)
	if err != nil {
		return fmt.Errorf("failed to connect to RabbitMQ: %w", err)
	}
	defer func() { _ = env.CloseConnections(context.Background()) }()

	requester, err := conn.NewRequester(ctx, &rmq.RequesterOptions{
		RequestQueueName: requestQueue,
		SettleStrategy:   rmq.DirectReplyTo,
	})
	if err != nil {
		return fmt.Errorf("failed to create requester: %w", err)
	}
	defer func() { _ = requester.Close(context.Background()) }()

	replyAddr, err := requester.GetReplyQueue()
	if err != nil {
		return err
	}
	log.Printf("RPC Client: Reply address: %s", replyAddr)

	log.Println("RPC Client: Started and ready to send requests...")

	requestID := 0
	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			log.Println("RPC Client: Shutting down...")
			return nil
		case <-ticker.C:
			requestID++
			replyCh, err := requester.Publish(ctx, requester.Message([]byte("ping")))
			if err != nil {
				log.Printf("RPC Client: Error sending request: %v", err)
				continue
			}
			log.Printf("RPC Client: Sent ping request (%d)", requestID)

			reply := <-replyCh
			if reply == nil {
				log.Printf("RPC Client: No reply for request %d", requestID)
				continue
			}
			var payload string
			if len(reply.Data) > 0 {
				payload = string(reply.Data[0])
			}
			log.Printf("RPC Client: Received reply - %s", payload)
		}
	}
}

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	log.Println("Starting Direct Reply-To RPC example...")
	log.Println("Make sure RabbitMQ is running on localhost:5672")

	go func() {
		if err := rpcServer(ctx); err != nil {
			log.Printf("RPC Server error: %v", err)
			cancel()
		}
	}()

	time.Sleep(500 * time.Millisecond)

	go func() {
		if err := rpcClient(ctx); err != nil {
			log.Printf("RPC Client error: %v", err)
			cancel()
		}
	}()

	sigs := make(chan os.Signal, 1)
	signal.Notify(sigs, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-sigs
		log.Println("\nReceived interrupt, shutting down...")
		cancel()
	}()

	<-ctx.Done()
	log.Println("Application shutting down...")
}

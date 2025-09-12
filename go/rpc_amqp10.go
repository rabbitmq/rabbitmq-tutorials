package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/Azure/go-amqp"
)

const (
	amqpURL = "amqp://guest:guest@localhost:5672/"
	// Request queue name for RPC requests
	requestQueue = "rpc-requests"
)

// RPC Server - handles ping requests and sends pong replies
func rpcServer(ctx context.Context) error {
	// Connect to RabbitMQ
	conn, err := amqp.Dial(ctx, amqpURL, nil)
	if err != nil {
		return fmt.Errorf("failed to connect to RabbitMQ: %w", err)
	}
	defer conn.Close()

	// Create a session
	session, err := conn.NewSession(ctx, nil)
	if err != nil {
		return fmt.Errorf("failed to create session: %w", err)
	}
	defer session.Close(ctx)

	// Create a receiver for the request queue
	receiver, err := session.NewReceiver(ctx, requestQueue, nil)
	if err != nil {
		return fmt.Errorf("failed to create receiver: %w", err)
	}
	defer receiver.Close(ctx)

	log.Println("RPC Server: Started and listening for requests...")

	for {
		select {
		case <-ctx.Done():
			log.Println("RPC Server: Shutting down...")
			return nil
		default:
			// Receive a request message
			msg, err := receiver.Receive(ctx, nil)
			if err != nil {
				log.Printf("RPC Server: Error receiving message: %v", err)
				continue
			}

			// Accept the message
			err = receiver.AcceptMessage(ctx, msg)
			if err != nil {
				log.Printf("RPC Server: Error accepting message: %v", err)
				continue
			}

			// Extract message properties

			messageID := msg.Properties.MessageID.(string)
			replyTo := *msg.Properties.ReplyTo

			log.Printf("RPC Server: Received ping request (ID: %s, ReplyTo: %s)", messageID, replyTo)

			// Check if we have a reply-to address
			if replyTo == "" {
				log.Println("RPC Server: No reply-to address, skipping reply")
				continue
			}

			// Create a sender for the reply
			sender, err := session.NewSender(ctx, replyTo, nil)
			if err != nil {
				log.Printf("RPC Server: Error creating sender for reply: %v", err)
				continue
			}

			// Create the pong reply message
			replyMsg := &amqp.Message{
				Properties: &amqp.MessageProperties{
					CorrelationID: messageID,
				},
				Data: [][]byte{[]byte("pong")},
			}

			// Send the reply
			err = sender.Send(ctx, replyMsg, nil)
			if err != nil {
				log.Printf("RPC Server: Error sending reply: %v", err)
			} else {
				log.Printf("RPC Server: Sent pong reply (CorrelationID: %s)", messageID)
			}

			// Close the sender
			sender.Close(ctx)
		}
	}
}

// RPC Client - sends ping requests using Direct Reply-To
func rpcClient(ctx context.Context) error {
	// Connect to RabbitMQ
	conn, err := amqp.Dial(ctx, amqpURL, nil)
	if err != nil {
		return fmt.Errorf("failed to connect to RabbitMQ: %w", err)
	}
	defer conn.Close()

	// Create a session
	session, err := conn.NewSession(ctx, nil)
	if err != nil {
		return fmt.Errorf("failed to create session: %w", err)
	}
	defer session.Close(ctx)

	// Create a receiver for Direct Reply-To
	receiver, err := session.NewReceiver(ctx, "", &amqp.ReceiverOptions{
		SourceCapabilities:        []string{"rabbitmq:volatile-queue"},
		SourceExpiryPolicy:        amqp.ExpiryPolicyLinkDetach,
		DynamicAddress:            true,
		RequestedSenderSettleMode: amqp.SenderSettleModeSettled.Ptr(),
	})
	if err != nil {
		return fmt.Errorf("failed to create Direct Reply-To receiver: %w", err)
	}
	defer receiver.Close(ctx)

	// Get the generated reply address from the receiver
	replyAddress := receiver.Address()
	log.Printf("RPC Client: Generated reply address: %s", replyAddress)

	// Create a sender for requests
	sender, err := session.NewSender(ctx, requestQueue, nil)
	if err != nil {
		return fmt.Errorf("failed to create sender: %w", err)
	}
	defer sender.Close(ctx)

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
			messageID := fmt.Sprintf("ping-%d", requestID)

			// Create the ping request message
			requestMsg := &amqp.Message{
				Properties: &amqp.MessageProperties{
					MessageID: messageID,
					ReplyTo:   &replyAddress,
				},
				Data: [][]byte{[]byte("ping")},
			}

			// Send the request
			err = sender.Send(ctx, requestMsg, nil)
			if err != nil {
				log.Printf("RPC Client: Error sending request: %v", err)
				continue
			}

			log.Printf("RPC Client: Sent ping request (ID: %s)", messageID)

			// Try to receive the reply with a timeout
			replyCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
			replyMsg, err := receiver.Receive(replyCtx, nil)
			cancel()

			if err != nil {
				log.Printf("RPC Client: Error receiving reply for %s: %v", messageID, err)
				continue
			}

			// Accept the reply message
			err = receiver.AcceptMessage(ctx, replyMsg)
			if err != nil {
				log.Printf("RPC Client: Error accepting reply: %v", err)
				continue
			}

			// Extract correlation ID and payload
			correlationID := ""
			if replyMsg.Properties != nil && replyMsg.Properties.CorrelationID != nil {
				correlationID = replyMsg.Properties.CorrelationID.(string)
			}

			replyPayload := string(replyMsg.Data[0])
			log.Printf("RPC Client: Received reply - %s (CorrelationID: %s)", replyPayload, correlationID)
		}
	}
}

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	log.Println("Starting Direct Reply-To RPC example...")
	log.Println("Make sure RabbitMQ is running on localhost:5672")

	// Start the RPC server in a goroutine
	go func() {
		if err := rpcServer(ctx); err != nil {
			log.Printf("RPC Server error: %v", err)
			cancel() // Cancel context to stop the client as well
		}
	}()

	// Start the RPC client in a goroutine
	go func() {
		if err := rpcClient(ctx); err != nil {
			log.Printf("RPC Client error: %v", err)
			cancel() // Cancel context to stop the server as well
		}
	}()

	// Wait for context cancellation (Ctrl+C or error)
	<-ctx.Done()
	log.Println("Application shutting down...")
}

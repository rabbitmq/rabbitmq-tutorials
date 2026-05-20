package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"strconv"
	"sync"
	"syscall"
	"time"

	amqp "github.com/rabbitmq/amqp091-go"
)

const MessageCount = 50000

func failOnError(err error, msg string) {
	if err != nil {
		log.Panicf("%s: %s", msg, err)
	}
}

// Create a channel in confirm mode on the given connection
func openConfirmChannel(conn *amqp.Connection) *amqp.Channel {
	ch, err := conn.Channel()
	failOnError(err, "Failed to open a channel")

	err = ch.Confirm(false)
	failOnError(err, "Failed to put channel into confirm mode")

	return ch
}

func main() {
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	failOnError(err, "Failed to connect to RabbitMQ")
	defer conn.Close()

	// Create a context that will be canceled on SIGINT (Ctrl+C) or SIGTERM.
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	handlePublisherConfirmsIndividually(ctx, conn)
	handlePublisherConfirmsInBatches(ctx, conn)
	handlePublisherConfirmsAsynchronously(ctx, conn)
}

// Handle publisher confirms individually (synchronous)
func handlePublisherConfirmsIndividually(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	confirmDeferred := ch.NotifyPublish(make(chan amqp.Confirmation, 1))

	ctx, cancel := context.WithTimeout(parentCtx, 60*time.Second)
	defer cancel()

	start := time.Now()
	for i := 0; i < MessageCount; i++ {
		body := strconv.Itoa(i)
		err = ch.PublishWithContext(ctx, "", q.Name, false, false, amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(body),
		})
		failOnError(err, "Failed to publish a message")

		// Wait for confirmation
		select {
		case confirm := <-confirmDeferred:
			if !confirm.Ack {
				log.Printf("Message %d was nacked by the broker", confirm.DeliveryTag)
			}
		case <-ctx.Done():
			log.Printf("Timeout waiting for confirmation: %v", ctx.Err())
			return
		}
	}

	duration := time.Since(start)
	log.Printf("Published %d messages and handled confirms individually in %v\n", MessageCount, duration)
}

// Handle publisher confirms in batches (synchronous)
func handlePublisherConfirmsInBatches(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	batchSize := 100

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	confirmDeferred := ch.NotifyPublish(make(chan amqp.Confirmation, batchSize))

	ctx, cancel := context.WithTimeout(parentCtx, 10*time.Second)
	defer cancel()

	outstandingMessageCount := 0
	start := time.Now()

	for i := 0; i < MessageCount; i++ {
		body := strconv.Itoa(i)
		err = ch.PublishWithContext(ctx, "", q.Name, false, false, amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(body),
		})
		failOnError(err, "Failed to publish a message")

		outstandingMessageCount++

		if outstandingMessageCount == batchSize {
			// Wait for all confirms for this batch
			for j := 0; j < batchSize; j++ {
				select {
				case confirm := <-confirmDeferred:
					if !confirm.Ack {
						log.Printf("Message %d in batch was nacked", confirm.DeliveryTag)
					}
				case <-ctx.Done():
					log.Printf("Timeout waiting for batch confirmation: %v", ctx.Err())
					return
				}
			}
			outstandingMessageCount = 0
		}
	}

	// Wait for remaining confirms left over in the final uneven batch
	if outstandingMessageCount > 0 {
		for j := 0; j < outstandingMessageCount; j++ {
			select {
			case confirm := <-confirmDeferred:
				if !confirm.Ack {
					log.Printf("Message %d in batch was nacked", confirm.DeliveryTag)
				}
			case <-ctx.Done():
				log.Printf("Timeout waiting for remaining batch confirmation: %v", ctx.Err())
				return
			}
		}
	}

	duration := time.Since(start)
	log.Printf("Published %d messages and handled confirms in batches in %v\n", MessageCount, duration)
}

// Handle publisher confirms asynchronously
func handlePublisherConfirmsAsynchronously(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	confirmDeferred := ch.NotifyPublish(make(chan amqp.Confirmation, MessageCount))

	var outstandingConfirms sync.Map
	var wg sync.WaitGroup
	wg.Add(1)

	ctx, cancel := context.WithTimeout(parentCtx, 10*time.Second)
	defer cancel()

	// Goroutine responsible for listening to incoming confirms asynchronously
	go func() {
		defer wg.Done()
		confirmedCount := 0
		for {
			select {
			case confirm := <-confirmDeferred:
				// Clean up map when confirms received
				if body, exists := outstandingConfirms.LoadAndDelete(confirm.DeliveryTag); exists {
					if !confirm.Ack {
						log.Printf("Message with body %s has been nack-ed. Delivery tag: %d", body, confirm.DeliveryTag)
					}
				}
				confirmedCount++
				if confirmedCount == MessageCount {
					return
				}
			case <-ctx.Done():
				log.Printf("Timeout waiting for asynchronous confirmation: %v", ctx.Err())
				return
			}
		}
	}()

	start := time.Now()
	// seqNo starts tracking delivery tags on the channel
	var seqNo uint64 = 1

	for i := 0; i < MessageCount; i++ {
		// Explicitly check if context is canceled
		select {
		case <-ctx.Done():
			wg.Wait() // Wait for the listener goroutine to exit
			return
		default:
			// Context is active, proceed
		}
		body := strconv.Itoa(i)
		outstandingConfirms.Store(seqNo, body)
		seqNo++

		err = ch.PublishWithContext(ctx, "", q.Name, false, false, amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(body),
		})
		failOnError(err, "Failed to publish a message")
	}

	// Wait for the async goroutine listener to finish processing all confirmations
	wg.Wait()

	duration := time.Since(start)
	log.Printf("Published %d messages and handled confirms asynchronously in %v\n", MessageCount, duration)
}

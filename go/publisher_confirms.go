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

// Per-wait confirmation timeout; must not be lower than the heartbeat timeout
const confirmTimeout = 5 * time.Second

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

// Handle publisher confirms individually (synchronous).
// The slowest option, consider using other options instead.
func handlePublisherConfirmsIndividually(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	start := time.Now()
	for i := 0; i < MessageCount; i++ {
		body := strconv.Itoa(i)
		confirm, err := ch.PublishWithDeferredConfirmWithContext(parentCtx, "", q.Name, false, false, amqp.Publishing{
			ContentType: amqp.MimeTextPlain,
			Body:        []byte(body),
		})
		failOnError(err, "Failed to publish a message")

		// Wait for confirmation
		ctx, cancel := context.WithTimeout(parentCtx, confirmTimeout)
		acked, err := confirm.WaitContext(ctx)
		cancel()
		if err != nil {
			log.Printf("Confirmation wait failed: %v", err)
			return
		}
		if !acked {
			log.Printf("Message %d was nacked by the broker", confirm.DeliveryTag)
		}
	}

	log.Printf("Published %d messages and handled confirms individually in %v", MessageCount, time.Since(start))
}

// Handle publisher confirms in batches (synchronous).
// Each `DeferredConfirmation` identifies the exact message nacked.
func handlePublisherConfirmsInBatches(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	const batchSize = 100

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	waitForBatch := func(confirms []*amqp.DeferredConfirmation) bool {
		ctx, cancel := context.WithTimeout(parentCtx, confirmTimeout)
		defer cancel()
		for _, confirm := range confirms {
			acked, err := confirm.WaitContext(ctx)
			if err != nil {
				log.Printf("Batch confirmation wait failed: %v", err)
				return false
			}
			if !acked {
				log.Printf("Message %d in batch was nacked", confirm.DeliveryTag)
			}
		}
		return true
	}

	confirms := make([]*amqp.DeferredConfirmation, 0, batchSize)
	start := time.Now()

	for i := 0; i < MessageCount; i++ {
		body := strconv.Itoa(i)
		confirm, err := ch.PublishWithDeferredConfirmWithContext(parentCtx, "", q.Name, false, false, amqp.Publishing{
			ContentType: amqp.MimeTextPlain,
			Body:        []byte(body),
		})
		failOnError(err, "Failed to publish a message")

		confirms = append(confirms, confirm)
		if len(confirms) == batchSize {
			// Wait for all confirms for this batch
			if !waitForBatch(confirms) {
				return
			}
			confirms = confirms[:0]
		}
	}

	// Wait for remaining confirms left over in the final uneven batch
	if len(confirms) > 0 && !waitForBatch(confirms) {
		return
	}

	log.Printf("Published %d messages and handled confirms in batches in %v", MessageCount, time.Since(start))
}

// Handle publisher confirms asynchronously: use this option when possible.
func handlePublisherConfirmsAsynchronously(parentCtx context.Context, conn *amqp.Connection) {
	ch := openConfirmChannel(conn)
	defer ch.Close()

	q, err := ch.QueueDeclare("", false, false, true, false, nil)
	failOnError(err, "Failed to declare a queue")

	// A small buffer suffices: the goroutine below drains it continuously
	confirms := ch.NotifyPublish(make(chan amqp.Confirmation, 100))

	var outstandingConfirms sync.Map
	var wg sync.WaitGroup
	wg.Add(1)

	// Goroutine responsible for listening to incoming confirms asynchronously
	go func() {
		defer wg.Done()
		// The library expands multiple-acks into one Confirmation per message
		for confirmed := 0; confirmed < MessageCount; confirmed++ {
			select {
			case confirm := <-confirms:
				// Clean up map when confirms received
				if body, ok := outstandingConfirms.LoadAndDelete(confirm.DeliveryTag); ok && !confirm.Ack {
					log.Printf("Message with body %s was nacked. Delivery tag: %d", body, confirm.DeliveryTag)
				}
			case <-time.After(confirmTimeout):
				log.Printf("No confirmation received within %v, giving up", confirmTimeout)
				return
			case <-parentCtx.Done():
				return
			}
		}
	}()

	start := time.Now()

	for i := 0; i < MessageCount; i++ {
		body := strconv.Itoa(i)
		// Correct only while a single goroutine publishes on this channel
		outstandingConfirms.Store(ch.GetNextPublishSeqNo(), body)

		err := ch.PublishWithContext(parentCtx, "", q.Name, false, false, amqp.Publishing{
			ContentType: amqp.MimeTextPlain,
			Body:        []byte(body),
		})
		if err != nil {
			log.Printf("Failed to publish: %v", err)
			break
		}
	}

	// Wait for the async goroutine listener to finish processing all confirmations
	wg.Wait()
	log.Printf("Published %d messages and handled confirms asynchronously in %v", MessageCount, time.Since(start))
}

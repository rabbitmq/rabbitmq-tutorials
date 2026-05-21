package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	amqp "github.com/rabbitmq/amqp091-go"
)

func failOnError(err error, msg string) {
	if err != nil {
		log.Panicf("%s: %s", msg, err)
	}
}

func main() {
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	failOnError(err, "Failed to connect to RabbitMQ")
	defer conn.Close()

	ch, err := conn.Channel()
	failOnError(err, "Failed to open a channel")
	defer ch.Close()

	confirms := make(chan amqp.Confirmation)
	ch.NotifyPublish(confirms)
	go func() {
		for confirm := range confirms {
			if confirm.Ack {
				// code when messages is confirmed
				log.Printf("Confirmed")
			} else {
				// code when messages is nack-ed
				log.Printf("Nacked")
			}
		}
	}()

	err = ch.Confirm(false)
	failOnError(err, "Failed to confirm")

	q, err := ch.QueueDeclare(
		"",    // name
		false, // durability
		false, // delete when unused
		true,  // exclusive
		false, // no-wait
		nil,   // arguments
	)
	failOnError(err, "Failed to declare a queue")

	consume(ch, q.Name)
	publish(ch, q.Name, "hello")

	log.Printf(" [*] Waiting for messages. To exit press CTRL+C")
	// Create a channel to receive OS signals
	c := make(chan os.Signal, 1)
	// Notify the channel for SIGINT (CTRL+C) and SIGTERM
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	// Block until a signal is received
	<-c
	log.Printf("Shutting down gracefully...")
	// Deferred conn.Close() and ch.Close() will execute!
}

func consume(ch *amqp.Channel, qName string) {
	msgs, err := ch.Consume(
		qName, // queue
		"",    // consumer
		true,  // auto-ack
		false, // exclusive
		false, // no-local
		false, // no-wait
		nil,   // args
	)
	failOnError(err, "Failed to register a consumer")

	go func() {
		for d := range msgs {
			log.Printf("Received a message: %s", d.Body)
		}
	}()
}

func publish(ch *amqp.Channel, qName, text string) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	err := ch.PublishWithContext(ctx, "", qName, false, false, amqp.Publishing{
		ContentType: "text/plain",
		Body:        []byte(text),
	})
	failOnError(err, "Failed to publish a message")
}

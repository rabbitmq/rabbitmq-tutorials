package main

import (
	"github.com/streadway/amqp"
	"log"
	"os"
	"fmt"
)

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
		panic(fmt.Sprintf("%s: %s", msg, err))
	}
}

func main() {
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	failOnError(err, "Failed to connect to RabbitMQ")
	defer conn.Close()

	ch, err := conn.Channel()
	failOnError(err, "Failed to open a channel")

	defer ch.Close()

	q, err := ch.QueueDeclare(
		"hello", // name
		false,   // durable
		false,   // delete when usused
		false,   // exclusive
		false,   // noWait
		nil,     // arguments
	)
	failOnError(err, "Failed to declare a queue")

	msgs, err := ch.Consume(q.Name, "", true, false, false, false, nil)
	failOnError(err, "Failed to register a consumer")

	done := make(chan bool)
	var d amqp.Delivery

	go func() {
		for d = range msgs {
			log.Printf("Received a message: %s", d.Body)
			done <- true
		}
	}()

	log.Printf(" [*] Waiting for messages. To exit press CTRL+C")
	select {
	case <-done:
		break
	}
	log.Printf("Done")

	os.Exit(0)
}

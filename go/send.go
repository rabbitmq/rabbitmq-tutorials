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

	body := "hello"
	err = ch.Publish(
		"",         // exchange
		q.Name,     // routing key
		false,      // mandatory
		false,      // immediate
		amqp.Publishing{
			ContentType:     "text/plain",
			Body:            []byte(body),
		})
	failOnError(err, "Failed to publish a message")

	os.Exit(0)
}

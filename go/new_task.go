package main

import (
	"github.com/streadway/amqp"
	"log"
	"os"
	"fmt"
	"strings"
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

	body := bodyFrom(os.Args)
	err = ch.Publish(
		"",           // exchange
		"task_queue", // routing key
		false,        // mandatory
		false,
		amqp.Publishing {
		        DeliveryMode:  amqp.Persistent,
			ContentType:     "text/plain",
			Body:            []byte(body),
		})
	failOnError(err, "Failed to publish a message")

	os.Exit(0)
}

func bodyFrom(args []string) string {
	var s string
	if (len(args) < 2) || os.Args[2] == "" {
		s = "hello"
	} else {
		s = strings.Join(args[1:], " ")

	}

	return s
}

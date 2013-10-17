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

	err = ch.ExchangeDeclare(
		"logs_direct", // name
		"direct",      // type
		true,          // durable
		false,         // auto-deleted
		false,         // internal
		false,         // noWait
		nil,           // arguments
	)
	failOnError(err, "Failed to declare an exchange")

	body := bodyFrom(os.Args)
	err = ch.Publish(
		"logs_direct",         // exchange
		severityFrom(os.Args), // routing key
		false,                 // mandatory
		false,                 // immediate
		amqp.Publishing{
			ContentType:     "text/plain",
			Body:            []byte(body),
		})

	failOnError(err, "Failed to publish a message")
	log.Printf(" [x] Sent %s", body)

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

func severityFrom(args []string) string {
	var s string

	if (len(args) < 1) || os.Args[1] == "" {
		s = "info"
	} else {
		s = os.Args[1]

	}

	return s
}
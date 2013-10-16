package main

import (
	"github.com/streadway/amqp"
	"log"
	"os"
)

func main() {
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	if err != nil {
		log.Fatalf("Dial: %s", err)
		return
	}
	defer conn.Close()

	ch, err := conn.Channel()
	if err != nil {
		log.Fatalf("Channel: %s", err)
		return
	}

	defer ch.Close()

	err = ch.ExchangeDeclare(
		"logs",    // name
		"fanout",  // type
		true,      // durable
		false,     // auto-deleted
		false,     // internal
		false,     // noWait
		nil,       // arguments
	)
	if err != nil {
		log.Fatalf("Exchange Declare: %s", err)
		return
	}

	var body string
	if os.Args[1] == "" {
		body = "hello"
	} else {
		body = os.Args[1]
		
	}
	err = ch.Publish(
		"logs", // exchange
		"",     // routing key
		false,  // mandatory
		false,  // immediate
		amqp.Publishing{
			ContentType:     "text/plain",
			Body:            []byte(body),
		})

	if err != nil {
		log.Fatalf("Exchange Publish: %s", err)
		return
	}
	log.Printf(" [x] Sent %s", body)

	os.Exit(0)
}

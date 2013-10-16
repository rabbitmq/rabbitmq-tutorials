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

	q, err := ch.QueueDeclare(
		"hello", // name
		false,   // durable
		false,   // delete when usused
		false,   // exclusive
		false,   // noWait
		nil,     // arguments
	)
	if err != nil {
		log.Fatalf("Queue Declare: %s", err)
		return
	}

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

	if err != nil {
		log.Fatalf("Exchange Publish: %s", err)
		return
	}

	os.Exit(0)
}

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

	msgs, err := ch.Consume(q.Name, "", true, false, false, false, nil)

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

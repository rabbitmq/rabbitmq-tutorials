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
		"task_queue", // name
		true,    // durable
		false,   // delete when unused
		false,   // exclusive
		false,   // noWait
		nil,     // arguments
	)
	failOnError(err, "Failed to declare a queue")

	ch.Qos(3, 0, false)

	msgs, err := ch.Consume(q.Name, "", false, false, false, false, nil)
	failOnError(err, "Failed to register a consumer")

	done := make(chan bool)
	var d amqp.Delivery

	go func() {
		for d = range msgs {
			log.Printf("Received a message: %s", d.Body)
			d.Ack(false)
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

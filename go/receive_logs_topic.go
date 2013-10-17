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

	err = ch.ExchangeDeclare(
		"logs_topic", // name
		"topic",      // type
		true,         // durable
		false,        // auto-deleted
		false,        // internal
		false,        // noWait
		nil,          // arguments
	)
	failOnError(err, "Failed to declare an exchange")
	q, err := ch.QueueDeclare(
		"",    // name
		false, // durable
		false, // delete when usused
		false, // exclusive
		false, // noWait
		nil,   // arguments
	)
	failOnError(err, "Failed to declare a queue")

	var s string
	for _, s = range os.Args {
		log.Printf("Binding queue %s to exchange %s with routing key %s", q.Name, "logs_topic", s)
		err = ch.QueueBind(
			q.Name,        // queue name
			s,             // routing key
			"logs_topic", // exchange
			false,
		        nil)
		failOnError(err, "Failed to bind a queue")
	}

	msgs, err := ch.Consume(q.Name, "", true, false, false, false, nil)

	done := make(chan bool)
	var d amqp.Delivery

	go func() {
		for d = range msgs {
			log.Printf(" [x] %s", d.Body)
			done <- true
		}
	}()

	log.Printf(" [*] Waiting for logs. To exit press CTRL+C")
	select {
	case <-done:
		break
	}
	log.Printf("Done")

	os.Exit(0)
}

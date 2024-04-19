package main

import (
	"bufio"
	"fmt"
	"github.com/rabbitmq/rabbitmq-stream-go-client/pkg/amqp"
	"github.com/rabbitmq/rabbitmq-stream-go-client/pkg/stream"
	"os"
)

func CheckErrSend(err error) {
	if err != nil {
		fmt.Printf("%s ", err)
		os.Exit(1)
	}
}
func main() {
	env, err := stream.NewEnvironment(
		stream.NewEnvironmentOptions().
			SetHost("localhost").
			SetPort(5552).
			SetUser("guest").
			SetPassword("guest"))
	CheckErrSend(err)

	streamName := "hello-go-stream"
	err = env.DeclareStream(streamName,
		&stream.StreamOptions{
			MaxLengthBytes: stream.ByteCapacity{}.GB(2),
		},
	)
	CheckErrSend(err)

	producer, err := env.NewProducer(streamName, stream.NewProducerOptions())
	CheckErrSend(err)

	// Send a message
	err = producer.Send(amqp.NewMessage([]byte("Hello world")))
	CheckErrSend(err)
	fmt.Printf(" [x] 'Hello world' Message sent\n")

	reader := bufio.NewReader(os.Stdin)
	fmt.Println(" [x] Press enter to close the producer")
	_, _ = reader.ReadString('\n')
	err = producer.Close()
	CheckErrSend(err)
}

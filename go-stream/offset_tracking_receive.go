package main

import (
	"errors"
	"fmt"
	"github.com/rabbitmq/rabbitmq-stream-go-client/pkg/amqp"
	"github.com/rabbitmq/rabbitmq-stream-go-client/pkg/stream"
	"os"
	"sync/atomic"
)

func main() {
	env, err := stream.NewEnvironment(
		stream.NewEnvironmentOptions().
			SetHost("localhost").
			SetPort(5552).
			SetUser("guest").
			SetPassword("guest"))
	CheckErrReceive(err)

	streamName := "stream-offset-tracking-go"
	err = env.DeclareStream(streamName,
		&stream.StreamOptions{
			MaxLengthBytes: stream.ByteCapacity{}.GB(2),
		},
	)
	CheckErrReceive(err)

	var firstOffset int64 = -1
	var messageCount int64 = -1
	var lastOffset atomic.Int64
	ch := make(chan bool)
	messagesHandler := func(consumerContext stream.ConsumerContext, message *amqp.Message) {
		if atomic.CompareAndSwapInt64(&firstOffset, -1, consumerContext.Consumer.GetOffset()) {
			fmt.Println("First message received.")
		}
		if atomic.AddInt64(&messageCount, 1)%10 == 0 {
			_ = consumerContext.Consumer.StoreOffset()
		}
		if string(message.GetData()) == "marker" {
			lastOffset.Store(consumerContext.Consumer.GetOffset())
			_ = consumerContext.Consumer.StoreOffset()
			_ = consumerContext.Consumer.Close()
			ch <- true
		}
	}

	var offsetSpecification stream.OffsetSpecification
	consumerName := "offset-tracking-tutorial"
	storedOffset, err := env.QueryOffset(consumerName, streamName)
	if errors.Is(err, stream.OffsetNotFoundError) {
		offsetSpecification = stream.OffsetSpecification{}.First()
	} else {
		offsetSpecification = stream.OffsetSpecification{}.Offset(storedOffset + 1)
	}

	_, err = env.NewConsumer(streamName, messagesHandler,
		stream.NewConsumerOptions().
			SetManualCommit().
			SetConsumerName(consumerName).
			SetOffset(offsetSpecification))
	CheckErrReceive(err)
	fmt.Println("Started consuming...")
	_ = <-ch
	fmt.Printf("Done consuming, first offset %d, last offset %d.\n", firstOffset, lastOffset.Load())
}

func CheckErrReceive(err error) {
	if err != nil {
		fmt.Printf("%s ", err)
		os.Exit(1)
	}
}

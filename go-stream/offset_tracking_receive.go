package main

import (
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

	var firstOffset, messageCount int64 = -1, 0
	var lastOffset atomic.Int64
	ch := make(chan bool)
	messagesHandler := func(consumerContext stream.ConsumerContext, message *amqp.Message) {
		if atomic.CompareAndSwapInt64(&firstOffset, -1, consumerContext.Consumer.GetOffset()) {
			fmt.Println("First message received.")
		}
		if atomic.AddInt64(&messageCount, 1)%10 == 0 {
			consumerContext.Consumer.StoreOffset()
			fmt.Println("storing offset")
		}
		if string(message.GetData()) == "marker" {
			lastOffset.Store(consumerContext.Consumer.GetOffset())
			consumerContext.Consumer.StoreOffset()
			consumerContext.Consumer.Close()
			ch <- true
		}
	}

	_, err = env.NewConsumer(streamName, messagesHandler,
		stream.NewConsumerOptions().
			SetManualCommit().
			SetConsumerName("offset-tracking-tutorial").
			SetOffset(stream.OffsetSpecification{}.LastConsumed()))
	CheckErrReceive(err)
	fmt.Println("Started consuming...")
	_ = <-ch

	fmt.Printf("Done consuming, first offset %d, last offset %d.\n", firstOffset, lastOffset.Load())
	CheckErrReceive(err)

}

func CheckErrReceive(err error) {
	if err != nil {
		fmt.Printf("%s ", err)
		os.Exit(1)
	}
}

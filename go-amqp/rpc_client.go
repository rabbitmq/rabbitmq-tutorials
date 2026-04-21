package main

import (
	"context"
	"errors"
	"log"
	"os"
	"strconv"
	"strings"

	rmq "github.com/rabbitmq/rabbitmq-amqp-go-client/pkg/rabbitmqamqp"
)

const brokerURI = "amqp://guest:guest@localhost:5672/"

func fibonacciRPC(n int) (res int, err error) {
	ctx := context.Background()
	env := rmq.NewEnvironment(brokerURI, nil)
	conn, err := env.NewConnection(ctx)
	if err != nil {
		return 0, err
	}
	defer func() {
		_ = env.CloseConnections(context.Background())
	}()

	requester, err := conn.NewRequester(ctx, &rmq.RequesterOptions{
		RequestQueueName: "rpc_queue",
	})
	if err != nil {
		return 0, err
	}
	defer func() { _ = requester.Close(context.Background()) }()

	replyCh, err := requester.Publish(ctx, requester.Message([]byte(strconv.Itoa(n))))
	if err != nil {
		return 0, err
	}
	reply := <-replyCh
	if reply == nil {
		return 0, errors.New("no reply received")
	}
	var payload []byte
	if len(reply.Data) > 0 {
		payload = reply.Data[0]
	}
	return strconv.Atoi(string(payload))
}

func main() {
	n := bodyFrom(os.Args)

	log.Printf(" [x] Requesting fib(%d)", n)
	res, err := fibonacciRPC(n)
	if err != nil {
		log.Panicf("Failed to handle RPC request: %v", err)
	}

	log.Printf(" [.] Got %d", res)
}

func bodyFrom(args []string) int {
	var s string
	if (len(args) < 2) || args[1] == "" {
		s = "30"
	} else {
		s = strings.Join(args[1:], " ")
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		log.Panicf("Failed to convert arg to integer: %v", err)
	}
	return n
}

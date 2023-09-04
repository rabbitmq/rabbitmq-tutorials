package main

import (
	"os"

	"github.com/rabbitmq/rabbitmq-tutorials/go/tutorial-1/receive"
	"github.com/rabbitmq/rabbitmq-tutorials/go/tutorial-1/send"
)

func main() {
	arg1 := os.Args[1]

	if arg1 == "send" {
		send.Send()
	}

	if arg1 == "receive" {
		receive.Receive()
	}
}

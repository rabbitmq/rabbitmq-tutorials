#! /usr/bin/env elixir
require Logger

# Installing the rabbitmq_stream Library
Mix.install([
  {:rabbitmq_stream, "~> 0.4.1"}
])

# First we start a Connection to the RabbitMQ Server
{:ok, connection} = RabbitMQStream.Connection.start_link()

# We can assume the stream doesn't exist yet, and attempt to create it. If it already exists,
# it should be still be good to go.
RabbitMQStream.Connection.create_stream(connection, "my_stream")

# Before publishing a message, we need to declare a producer. It is required by the
# RabbitMQ Sever to prevent message duplication.
{:ok, producer_id} =
  RabbitMQStream.Connection.declare_producer(connection, "my_stream", "my_producer")

# Each producer has a sequence number, that must must be published with the message, and
# incremented after each message.
{:ok, sequence_number} =
  RabbitMQStream.Connection.query_producer_sequence(connection, "my_stream", "my_producer")

# Now we can publish a message. Note that we only specify the producer_id and sequence number.
# The target Stream is already tracked by the server based on the producer_id.
:ok =
  RabbitMQStream.Connection.publish(connection, producer_id, sequence_number + 1, "Hello, World!")

Logger.info("Published: \"Hello, World!\"")

#! /usr/bin/env elixir
require Logger

# Installing the rabbitmq_stream Library
Mix.install([
  {:rabbitmq_stream, "~> 0.4.1"}
])

# First we start a Connection to the RabbitMQ Server
{:ok, connection} = RabbitMQStream.Connection.start_link()

stream_name = "stream-offset-tracking-elixir"
producer_reference = "offset-tracking-producer"

# We can assume the stream doesn't exist yet, and attempt to create it. If it already exists,
# it should be still be good to go.
RabbitMQStream.Connection.create_stream(connection, stream_name)

# Before publishing a message, we need to declare a producer. It is required by the
# RabbitMQ Server to prevent message duplication.
{:ok, producer_id} =
  RabbitMQStream.Connection.declare_producer(connection, stream_name, producer_reference)

# Each producer has a sequence number, that must be published with the message, and
# incremented after each message.
{:ok, sequence_number} =
  RabbitMQStream.Connection.query_producer_sequence(connection, stream_name, producer_reference)

message_count = 100

Logger.info("Publishing #{message_count} messages...")

for i <- 1..message_count do
  # The last message is a "marker", letting the consumer know it has caught up with
  # every message published during this run.
  body = if i == message_count, do: "marker", else: "hello"

  :ok = RabbitMQStream.Connection.publish(connection, producer_id, sequence_number + i, body)
end

# `publish/5` doesn't wait for the server's confirmation, so we poll the sequence number
# the server tracks for this producer, until it catches up with the last message we sent.
# This tells us every message has been safely persisted to the stream.
last_sequence_number = sequence_number + message_count

wait_for_confirmation = fn wait_for_confirmation ->
  {:ok, confirmed_sequence_number} =
    RabbitMQStream.Connection.query_producer_sequence(connection, stream_name, producer_reference)

  unless confirmed_sequence_number >= last_sequence_number do
    Process.sleep(50)
    wait_for_confirmation.(wait_for_confirmation)
  end
end

wait_for_confirmation.(wait_for_confirmation)

Logger.info("Messages confirmed.")

#! /usr/bin/env elixir
require Logger

# Installing the rabbitmq_stream Library
Mix.install([
  {:rabbitmq_stream, "~> 0.4.1"}
])

# First we start a Connection to the RabbitMQ Server
{:ok, connection} = RabbitMQStream.Connection.start_link()

# We can assume the stream already exists as this sample is meant to be run after the 'publish.exs' sample

# Now we can subscribe to the stream, receiving up to 1 chunk.
{:ok, subscription_id} =
  RabbitMQStream.Connection.subscribe(connection, "my_stream", self(), :first, 1)

# Now we can consume the messages
receive do
  # Each 'deliver' data comes inside a Chunk, which may contain multiple messages
  {:deliver, %{subscription_id: ^subscription_id, osiris_chunk: chunk}} ->
    for message <- chunk.data_entries do
      Logger.info("Received: #{inspect(message)}")
    end
end

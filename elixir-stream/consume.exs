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

# subscribe/5 must be called from the process that will run `receive`, since that's
# the process the server delivers messages to.
consumer_task =
  Task.async(fn ->
    # Subscribe to the stream, receiving up to 1 chunk.
    {:ok, subscription_id} =
      RabbitMQStream.Connection.subscribe(connection, "my_stream", self(), :first, 1)

    Stream.repeatedly(fn ->
      receive do
        # A chunk may contain multiple messages. We only have 1 credit, so we top it
        # back up after each chunk to keep them coming.
        {:deliver, %{subscription_id: ^subscription_id, osiris_chunk: chunk}} ->
          for message <- chunk.data_entries do
            Logger.info("Received: #{inspect(message)}")
          end

          RabbitMQStream.Connection.credit(connection, subscription_id, 1)
      end
    end)
    |> Stream.run()
  end)

IO.gets(" [x] Waiting for messages. Press enter to close the consumer\n")

Task.shutdown(consumer_task, :brutal_kill)

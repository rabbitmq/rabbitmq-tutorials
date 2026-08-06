#! /usr/bin/env elixir
require Logger

# Installing the rabbitmq_stream Library
Mix.install([
  {:rabbitmq_stream, "~> 0.4.1"}
])

# First we start a Connection to the RabbitMQ Server
{:ok, connection} = RabbitMQStream.Connection.start_link()

stream_name = "stream-offset-tracking-elixir"
consumer_reference = "offset-tracking-tutorial"

# We can assume the stream doesn't exist yet, and attempt to create it. If it already exists,
# it should be still be good to go.
RabbitMQStream.Connection.create_stream(connection, stream_name)

# If we already stored an offset under this reference in a previous run, resume right after
# it. Otherwise, start from the beginning of the stream.
offset_specification =
  case RabbitMQStream.Connection.query_offset(connection, stream_name, consumer_reference) do
    {:ok, stored_offset} -> {:offset, stored_offset + 1}
    {:error, :no_offset} -> :first
  end

{:ok, subscription_id} =
  RabbitMQStream.Connection.subscribe(connection, stream_name, self(), offset_specification, 10)

Logger.info("Started consuming...")

# Consumes chunks of messages until the "marker" message is found, storing the offset every
# 10 messages, and once more when the marker is reached.
consume = fn consume, message_count, first_offset, last_offset ->
  receive do
    {:deliver, %{subscription_id: ^subscription_id, osiris_chunk: chunk}} ->
      # Every entry in a chunk occupies one offset, starting at the chunk's own offset.
      entries = Enum.with_index(chunk.data_entries, chunk.chunk_id)

      {message_count, first_offset, last_offset, marker_found?} =
        Enum.reduce(entries, {message_count, first_offset, last_offset, false}, fn {data, offset},
                                                                                     {message_count, first_offset,
                                                                                      last_offset, _marker_found?} ->
          if first_offset == nil, do: Logger.info("First message received.")
          first_offset = first_offset || offset

          message_count = message_count + 1
          if rem(message_count, 10) == 0 do
            RabbitMQStream.Connection.store_offset(connection, stream_name, consumer_reference, offset)
          end

          if data == "marker" do
            RabbitMQStream.Connection.store_offset(connection, stream_name, consumer_reference, offset)
            {message_count, first_offset, offset, true}
          else
            {message_count, first_offset, last_offset, false}
          end
        end)

      if marker_found? do
        RabbitMQStream.Connection.unsubscribe(connection, subscription_id)
        {first_offset, last_offset}
      else
        # Replenish the credit spent on the chunk we just consumed, so the server keeps sending.
        RabbitMQStream.Connection.credit(connection, subscription_id, 1)
        consume.(consume, message_count, first_offset, last_offset)
      end
  end
end

{first_offset, last_offset} = consume.(consume, 0, nil, nil)

Logger.info("Done consuming, first offset #{first_offset}, last offset #{last_offset}.")

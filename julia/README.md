# Julia code for RabbitMQ tutorials

Here you can find Julia code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

To run this code you need to install the `AMQPClient` and `JSON`. To install it, run

``` julia
julia> using Pkg

julia> Pkg.add("AMQPClient")
    Updating registry at `~/.julia/registries/General`
   Resolving package versions...
  No Changes to `~/.julia/environments/v1.6/Project.toml`
  No Changes to `~/.julia/environments/v1.6/Manifest.toml`

julia> Pkg.add("JSON")
   Resolving package versions...
  No Changes to `~/.julia/environments/v1.6/Project.toml`
  No Changes to `~/.julia/environments/v1.6/Manifest.toml`

```

## Code

Tutorial one: "Hello World!"

    julia send.jl
    julia receive.jl


Tutorial two: Work Queues:

    julia new_task.jl "A very hard task which takes two seconds.."
    julia worker.jl


Tutorial three: Publish/Subscribe

    julia receive_logs.jl
    julia emit_log.jl "info: This is the log message"


Tutorial four: Routing

    julia receive_logs_direct.jl info
    julia emit_log_direct.jl info "The message"


Tutorial five: Topics

    julia receive_logs_topic.jl "*.rabbit"
    julia emit_log_topic.jl red.rabbit Hello


Tutorial six: RPC

    julia rpc_server.jl
    julia rpc_client.jl

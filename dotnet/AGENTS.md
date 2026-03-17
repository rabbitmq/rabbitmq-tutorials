# Agent Notes for These .NET Tutorials

## `Console.ReadLine` in non-TTY environments

Consumer programs (Receive, Worker, ReceiveLogs, ReceiveLogsDirect, ReceiveLogsTopic,
RPCServer) use `Console.ReadLine` to block until the user presses `[Enter]`.

When standard input is not connected to a TTY, `ReadLine()` returns `null` immediately and the process
exits before receiving any deliveries.

Keep stdin open in automated or background contexts by piping from a blocking process:

```sh
(while true; do sleep 20; done) | dotnet run --project Receive &
(while true; do sleep 20; done) | dotnet run --project ReceiveLogs &
```

The `Console.ReadLine()` and the "Press [enter] to
exit." must never be deleted as a workaround: they exist for interactive use.

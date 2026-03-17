# Agent Notes: Swift Tutorials

## Stdout Buffering (non-TTY environments)

Swift's `print` is block-buffered when stdout is not connected to a TTY. Consumer
binaries will connect and receive messages correctly but produce no visible output.

To work around this, set `NSUnbufferedIO=YES` before running
any compiled tutorial binary, like so:

```sh
NSUnbufferedIO=YES .build/debug/Receive &
NSUnbufferedIO=YES .build/debug/Worker &
NSUnbufferedIO=YES .build/debug/RPCServer &
```

This applies to all binaries, including producers, though producers are less affected
since they exit promptly after sending.

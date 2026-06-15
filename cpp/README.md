# C++ code for RabbitMQ tutorials

Here you can find the C++ examples for the [RabbitMQ tutorials](https://www.rabbitmq.com/tutorials).

These examples use [`rmqcpp`](https://github.com/bloomberg/rmqcpp), Bloomberg's
C++ client for RabbitMQ. `rmqcpp` is safe by default: publisher confirms and
consumer acknowledgements are always on, durable quorum queues and persistent
delivery are used for the long-lived queues, heartbeats run in the background,
and the client reconnects and re-declares its topology on its own. The tutorials
lean on those defaults rather than working around them.

## Requirements

To run the examples you need a RabbitMQ node listening on `localhost:5672` with
the default `guest`/`guest` user, which is the default for a local broker.

The examples are written in **C++17**, the standard that `rmqcpp` and its
underlying [BDE](https://github.com/bloomberg/bde) libraries are built and tested
against (the later editions are C++20 and C++23; there is no "C++24"). Building
against C++17 keeps the tutorials binary-compatible with a stock `rmqcpp` build.

## Pinned `rmqcpp` version

`rmqcpp` is vendored as a git submodule at `cpp/rmqcpp`, pinned to a fixed
upstream commit so the tutorials always build against a known revision:

```
commit cc6885319ccb97b8a6d13e09e83a52c43aab16c7
```

After cloning the tutorials repository, initialize the submodule before
building. The build uses `add_subdirectory(rmqcpp)`, so it must be checked out:

```bash
git submodule update --init --recursive
```

## Building

Make sure the `rmqcpp` submodule is initialized first (see above).

`rmqcpp` builds with [`vcpkg`](https://vcpkg.io), which supplies BDE, Boost.Asio,
OpenSSL and the other dependencies listed in `vcpkg.json`. Set `VCPKG_ROOT` to
your vcpkg checkout first, then configure and build with CMake:

```bash
cmake -B build -S . \
  -DCMAKE_TOOLCHAIN_FILE="${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake" \
  -DCMAKE_CXX_STANDARD=17
cmake --build build
```

The tutorial executables are written to `build/`. See the
[`rmqcpp` README](https://github.com/bloomberg/rmqcpp#building) for more detail
on the build environment, including a Docker-based setup.

## Running the tutorials

### Tutorial 1: Hello World

```bash
./build/receive
./build/send
```

### Tutorial 2: Work Queues

```bash
./build/worker
./build/new_task "A task that takes a while..."
```

### Tutorial 3: Publish/Subscribe

```bash
./build/receive_logs
./build/emit_log "info: a log message"
```

### Tutorial 4: Routing

```bash
./build/receive_logs_direct info warning error
./build/emit_log_direct error "a fatal error"
```

### Tutorial 5: Topics

```bash
./build/receive_logs_topic "kern.*" "*.critical"
./build/emit_log_topic kern.critical "a critical kernel error"
```

### Tutorial 6: RPC

```bash
./build/rpc_server
./build/rpc_client 30
```

### Tutorial 7: Publisher Confirms

```bash
./build/publisher_confirms
```

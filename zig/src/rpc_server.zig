const std = @import("std");
const bunny = @import("bunny");

fn fibonacci(n: u64) u64 {
    if (n == 0 or n == 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.smp_allocator;
    const io = init.io;

    var stdout_buf: [256]u8 = undefined;
    var stdout_w = std.Io.File.stdout().writerStreaming(io, &stdout_buf);
    const out = &stdout_w.interface;
    defer out.flush() catch {};

    const conn = try bunny.Connection.open(allocator, .{});
    defer conn.deinit();

    const ch = try conn.openChannel();
    defer ch.close();

    const queue = try ch.quorumQueue("rpc_queue");
    try ch.prefetch(1);
    _ = try queue.subscribe(.manual);

    try out.print(" [x] Awaiting RPC requests\n", .{});
    try out.flush();

    while (try ch.recvDelivery()) |delivery| {
        defer delivery.deinit(allocator);

        const n = std.fmt.parseInt(u64, delivery.body, 10) catch 0;
        const result = fibonacci(n);

        var buf: [32]u8 = undefined;
        const reply_body = try std.fmt.bufPrint(&buf, "{d}", .{result});

        delivery.respond(reply_body) catch |err| switch (err) {
            error.NoReplyTo => {},
            else => return err,
        };

        try delivery.ack();
    }
}

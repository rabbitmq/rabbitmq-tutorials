const std = @import("std");
const bunny = @import("bunny");

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.smp_allocator;
    const io = init.io;
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    var stdout_buf: [256]u8 = undefined;
    var stdout_w = std.Io.File.stdout().writerStreaming(io, &stdout_buf);
    const out = &stdout_w.interface;
    defer out.flush() catch {};

    if (args.len < 2) {
        try out.print("Usage: {s} [info] [warning] [error]\n", .{args[0]});
        try out.flush();
        std.process.exit(1);
    }

    const conn = try bunny.Connection.open(allocator, .{
        .recovery = .{ .enabled = false },
    });
    defer conn.deinit();

    const ch = try conn.openChannel();
    defer ch.close();

    _ = try ch.declareDirectExchange("direct_logs");

    var queue = try ch.temporaryQueue();
    defer queue.deinit(allocator);

    for (args[1..]) |severity| {
        try queue.bind("direct_logs", severity);
    }

    _ = try queue.subscribe(.automatic);

    try out.print(" [*] Waiting for logs. To exit press CTRL+C\n", .{});
    try out.flush();

    while (try ch.recvDelivery()) |delivery| {
        defer delivery.deinit(allocator);
        try out.print(" [x] {s}:{s}\n", .{ delivery.routing_key, delivery.body });
        try out.flush();
    }
}

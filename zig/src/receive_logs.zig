const std = @import("std");
const bunny = @import("bunny");

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

    _ = try ch.declareFanoutExchange("logs");

    var queue = try ch.temporaryQueue();
    defer queue.deinit(allocator);

    try queue.bind("logs", "");
    _ = try queue.subscribe(.automatic);

    try out.print(" [*] Waiting for logs. To exit press CTRL+C\n", .{});
    try out.flush();

    while (try ch.recvDelivery()) |delivery| {
        defer delivery.deinit(allocator);
        try out.print(" [x] {s}\n", .{delivery.body});
        try out.flush();
    }
}

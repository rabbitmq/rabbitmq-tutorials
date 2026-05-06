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

    const n: u64 = if (args.len > 1)
        std.fmt.parseInt(u64, args[1], 10) catch 30
    else
        30;

    const conn = try bunny.Connection.open(allocator, .{
        .recovery = .{ .enabled = false },
    });
    defer conn.deinit();

    const ch = try conn.openChannel();
    defer ch.close();

    var reply_queue = try ch.temporaryQueue();
    defer reply_queue.deinit(allocator);
    _ = try reply_queue.subscribe(.automatic);

    const correlation_id = bunny.correlationId(io);

    var req_buf: [32]u8 = undefined;
    const request = try std.fmt.bufPrint(&req_buf, "{d}", .{n});

    try out.print(" [x] Requesting fib({d})\n", .{n});
    try out.flush();

    try ch.publishToQueue("rpc_queue", request, bunny.BasicProperties.default
        .withCorrelationId(&correlation_id)
        .withReplyTo(reply_queue.name));

    while (try ch.recvDelivery()) |delivery| {
        defer delivery.deinit(allocator);

        const cid = delivery.properties.correlation_id orelse continue;
        if (!std.mem.eql(u8, cid, &correlation_id)) continue;

        try out.print(" [.] Got {s}\n", .{delivery.body});
        return;
    }
}

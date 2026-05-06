const std = @import("std");
const bunny = @import("bunny");

const message_count: usize = 50_000;

fn nowNs(io: std.Io) i96 {
    return std.Io.Timestamp.now(io, .boot).nanoseconds;
}

fn publishIndividually(
    allocator: std.mem.Allocator,
    io: std.Io,
    conn: *bunny.Connection,
    out: *std.Io.Writer,
) !void {
    const ch = try conn.openChannel();
    defer ch.close();

    var queue = try ch.temporaryQueue();
    defer queue.deinit(allocator);

    try ch.confirmSelectWithOptions(.{ .tracking = true });

    const start = nowNs(io);
    var i: usize = 0;
    while (i < message_count) : (i += 1) {
        var buf: [32]u8 = undefined;
        const body = try std.fmt.bufPrint(&buf, "{d}", .{i});
        try queue.publish(body, .{});
    }
    const elapsed_ms = @divTrunc(nowNs(io) - start, std.time.ns_per_ms);

    try out.print("Published {d} messages individually in {d} ms\n", .{ message_count, elapsed_ms });
    try out.flush();
}

fn publishInBatch(
    allocator: std.mem.Allocator,
    io: std.Io,
    conn: *bunny.Connection,
    out: *std.Io.Writer,
) !void {
    const ch = try conn.openChannel();
    defer ch.close();

    var queue = try ch.temporaryQueue();
    defer queue.deinit(allocator);

    try ch.confirmSelect();

    const batch_size: usize = 1000;
    const bodies = try allocator.alloc([]const u8, batch_size);
    defer allocator.free(bodies);

    const scratch = try allocator.alloc(u8, batch_size * 16);
    defer allocator.free(scratch);

    const start = nowNs(io);
    var i: usize = 0;
    while (i < message_count) : (i += batch_size) {
        const this_batch = @min(batch_size, message_count - i);
        var off: usize = 0;
        for (0..this_batch) |j| {
            const body = try std.fmt.bufPrint(scratch[off..], "{d}", .{i + j});
            bodies[j] = body;
            off += body.len;
        }
        try ch.publishBatch(bodies[0..this_batch], .{ .routing_key = queue.name });
    }

    _ = try ch.waitForConfirms();
    const elapsed_ms = @divTrunc(nowNs(io) - start, std.time.ns_per_ms);

    try out.print("Published {d} messages in batch in {d} ms\n", .{ message_count, elapsed_ms });
    try out.flush();
}

fn publishAsynchronously(
    allocator: std.mem.Allocator,
    io: std.Io,
    conn: *bunny.Connection,
    out: *std.Io.Writer,
) !void {
    const ch = try conn.openChannel();
    defer ch.close();

    var queue = try ch.temporaryQueue();
    defer queue.deinit(allocator);

    // Tracking is off, so publishes do not block on individual confirms;
    // a single waitForConfirms at the end drains any outstanding ones.
    try ch.confirmSelect();

    const start = nowNs(io);
    var i: usize = 0;
    while (i < message_count) : (i += 1) {
        var buf: [32]u8 = undefined;
        const body = try std.fmt.bufPrint(&buf, "{d}", .{i});
        try queue.publish(body, .{});
    }

    _ = try ch.waitForConfirms();
    const elapsed_ms = @divTrunc(nowNs(io) - start, std.time.ns_per_ms);

    try out.print("Published {d} messages asynchronously in {d} ms\n", .{ message_count, elapsed_ms });
    try out.flush();
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

    try publishIndividually(allocator, io, conn, out);
    try publishInBatch(allocator, io, conn, out);
    try publishAsynchronously(allocator, io, conn, out);
}

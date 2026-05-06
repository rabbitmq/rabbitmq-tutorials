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

    const severity: []const u8 = if (args.len > 1) args[1] else "anonymous.info";
    const message = try messageFromArgs(allocator, args, 2);
    defer allocator.free(message);

    const conn = try bunny.Connection.open(allocator, .{
        .recovery = .{ .enabled = false },
    });
    defer conn.deinit();

    const ch = try conn.openChannel();
    defer ch.close();

    const exchange = try ch.declareTopicExchange("topic_logs");

    try exchange.publish(message, severity, .{});
    try out.print(" [x] Sent {s}:{s}\n", .{ severity, message });
}

fn messageFromArgs(allocator: std.mem.Allocator, args: []const [:0]const u8, start: usize) ![]u8 {
    if (start >= args.len) return allocator.dupe(u8, "Hello World!");
    const parts = try allocator.alloc([]const u8, args.len - start);
    defer allocator.free(parts);
    for (args[start..], 0..) |a, i| parts[i] = a;
    return std.mem.join(allocator, " ", parts);
}

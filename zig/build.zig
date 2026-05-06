const std = @import("std");

const tutorials = [_][]const u8{
    "send",
    "receive",
    "new_task",
    "worker",
    "emit_log",
    "receive_logs",
    "emit_log_direct",
    "receive_logs_direct",
    "emit_log_topic",
    "receive_logs_topic",
    "rpc_server",
    "rpc_client",
    "publisher_confirms",
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const bunny_dep = b.dependency("bunny", .{
        .target = target,
        .optimize = optimize,
    });
    const bunny_mod = bunny_dep.module("bunny");

    inline for (tutorials) |name| {
        const exe = b.addExecutable(.{
            .name = name,
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/" ++ name ++ ".zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "bunny", .module = bunny_mod },
                },
            }),
        });
        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| run_cmd.addArgs(args);

        const run_step = b.step("run-" ++ name, "Run the " ++ name ++ " tutorial");
        run_step.dependOn(&run_cmd.step);
    }
}

const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    try stdout.print("Hello, World!\nI am Zlox.\n", .{});
}

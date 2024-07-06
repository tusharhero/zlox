// The GPLv3 License (GPLv3)

// Copyright © 2024 tusharhero

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();
const max = std.math.maxInt(u64);

fn run(source_code: []u8) !void {
    // Just print source code for now.
    try stdout.print("{s}\n", .{source_code});
}

fn runFile(allocator: std.mem.Allocator, path: []u8) !void {
    const source_code = try std.fs.cwd()
        .readFileAlloc(allocator, path, max);
    defer allocator.free(source_code);
    try run(source_code);
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    while (b: {
        try stdout.print("> ", .{});
        break :b try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', max);
    }) |line| : (allocator.free(line)) {
        try run(line);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => try runPrompt(allocator),
        2 => try runFile(allocator, args[1]),
        else => try stdout.print("Usage: {s} [script]\n", .{args[0]}),
    }
}

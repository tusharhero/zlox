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
const Lexxer = @import("lexxer.zig").Lexxer;
const Parser = @import("parser.zig").Parser;
const Printer = @import("ast.zig").Printer;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();
const stderr = std.io.getStdErr().writer();

const max = std.math.maxInt(u64);

pub fn _error(line: u64, message: []const u8) !void {
    try report(line, "", message);
}

fn report(line: u64, where: []u8, message: []const u8) !void {
    try stderr.print(
        "[line {d} ] Error {s}: {s}\n",
        .{ line, where, message },
    );
}

fn run(allocator: std.mem.Allocator, source_code: []u8) !void {
    var lexxer = try Lexxer.init(allocator, source_code);
    defer lexxer.deinit();
    const tokens = try lexxer.scanTokens();
    var parser = try Parser.init(tokens);
    defer parser.deinit();
    const parsed_expression = try parser.expression();
    var printer = try Printer.init(Printer.Notation.parenthesized_prefix);
    defer printer.deinit();
    // Just print source code, tokens, and parenthesized expression for now.
    try stdout.print("{s}\n", .{source_code});
    try stdout.print("{any}\n", .{tokens.items});
    try stdout.print("{!s}\n", .{printer.printExpr(parsed_expression)});
}

fn runFile(allocator: std.mem.Allocator, path: []u8) !void {
    const cwd = std.fs.cwd();
    const source_code = cwd.readFileAlloc(
        allocator,
        path,
        max,
    ) catch |err| switch (err) {
        error.FileNotFound => {
            try stderr.print("Error: {s} not found\n", .{path});
            return;
        },
        else => return err,
    };
    defer allocator.free(source_code);
    try run(allocator, source_code);
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    while (b: {
        try stdout.print("> ", .{});
        break :b try stdin.readUntilDelimiterOrEofAlloc(
            allocator,
            '\n',
            max,
        );
    }) |line| : (allocator.free(line)) {
        try run(allocator, line);
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => try runPrompt(allocator),
        2 => try runFile(allocator, args[1]),
        else => {
            try stdout.print("Usage: {s} [script]\n", .{args[0]});
            return 2;
        },
    }
    return 0;
}

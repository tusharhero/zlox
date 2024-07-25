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
const Token = @import("tokens.zig").Token;
const Type = @import("tokens.zig").TokenType;
const Parser = @import("parser.zig").Parser;
const Printer = @import("ast.zig").Printer;
const Interpreter = @import("interpreter.zig").Interpreter;

pub const stdout = std.io.getStdOut().writer();
pub const stdin = std.io.getStdIn().reader();
pub const stderr = std.io.getStdErr().writer();

const max = std.math.maxInt(u64);

pub const Errors = error{
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    DeviceBusy,
    InvalidArgument,
    AccessDenied,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    WouldBlock,
    ConnectionResetByPeer,
    OutOfMemory,
    Unexpected,
};

pub fn _error(info: union(enum) { token: Token, line: u64 }, message: []const u8) !void {
    switch (info) {
        .token => |token| {
            if (token._type == Type.EOF) {
                try report(token.line, " at end", message);
            } else {
                try report(token.line, token.lexeme, message);
            }
        },
        .line => |line| {
            try report(line, "", message);
        },
    }
}

fn report(line: u64, where: []const u8, message: []const u8) !void {
    try stderr.print(
        "[line {d} ] Error {s}: {s}\n",
        .{ line, where, message },
    );
}

fn run(allocator: std.mem.Allocator, source_code: []u8, interpreter: *Interpreter) !void {
    var lexxer = try Lexxer.init(allocator);
    defer lexxer.deinit();
    const tokens = lexxer.scanTokens(source_code) catch return;
    var parser = try Parser.init();
    defer parser.deinit();
    const statements = parser.parse(tokens) catch return;
    var printer = try Printer.init(Printer.Notation.parenthesized_prefix);
    defer printer.deinit();
    interpreter.interpret(statements) catch return;
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

    var interpreter = try Interpreter.init(allocator, .{ .file = stdout });
    defer interpreter.deinit();

    try run(allocator, source_code, &interpreter);
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    var interpreter = try Interpreter.init(allocator, .{ .file = stdout });
    defer interpreter.deinit();
    while (b: {
        try stdout.print("> ", .{});
        break :b try stdin.readUntilDelimiterOrEofAlloc(
            allocator,
            '\n',
            max,
        );
    }) |line| : (allocator.free(line)) {
        try run(allocator, line, &interpreter);
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

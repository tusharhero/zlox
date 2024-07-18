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
const _tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const main = @import("main.zig");
const Token = _tokens.Token;
const Type = _tokens.TokenType;

const Object = union(enum) {
    nill: ?void,
    boolean: bool,
    number: f64,
    string: []const u8,
};

pub const Interpreter = struct {
    arena: std.heap.ArenaAllocator,

    const Error = error{
        InterpreterError,
    };
    const errors = error{
        InterpreterError,
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

    /// Caller must call deinit.
    pub fn init() Interpreter {
        return Interpreter{
            .arena = std.heap.ArenaAllocator.init(
                std.heap.page_allocator,
            ),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.arena.deinit();
    }

    fn isTruthy(self: *Interpreter, object: Object) Object {
        _ = self;
        const false_object = Object{
            .boolean = false,
        };
        const true_object = Object{
            .boolean = true,
        };
        const nill_object = Object{
            .nill = null,
        };
        if (object.boolean == false_object.boolean or
            object.nill == nill_object.nill) return false_object;
        return true_object;
    }

    fn evalUnary(self: *Interpreter, expression: ast.Unary) !Object {
        const right = try self.evaluate(expression.right);
        const operator = expression.operator._type;
        switch (operator) {
            Type.MINUS => {
                switch (right) {
                    .number => |num| return Object{ .number = -1 * num },
                    else => {
                        try main._error(
                            .{ .line = 0 },
                            "Unary operator negation is only available for numbers.",
                        );
                        return Error.InterpreterError;
                    },
                }
            },
            Type.BANG => return self.isTruthy(right),
            else => {
                try main._error(
                    .{ .line = 0 },
                    "Unkown unary operator.",
                );
                return Error.InterpreterError;
            },
        }
    }
    fn evalBinary(self: *Interpreter, expression: ast.Binary) !Object {
        const left = try self.evaluate(expression.left);
        const right = try self.evaluate(expression.right);
        if (@intFromEnum(left) != @intFromEnum(right)) return Error.InterpreterError;
        switch (left) {
            .number => {
                const l = left.number;
                const r = right.number;
                return switch (expression.operator._type) {
                    Type.PLUS => Object{ .number = l + r },
                    Type.MINUS => Object{ .number = l - r },
                    Type.STAR => Object{ .number = l * r },
                    Type.SLASH => Object{ .number = l / r },
                    else => Error.InterpreterError,
                };
            },
            .string => {
                const l = left.string;
                const r = right.string;
                return switch (expression.operator._type) {
                    Type.PLUS => Object{
                        .string = try std.fmt.allocPrint(
                            self.arena.allocator(),
                            "{s}{s}",
                            .{ l, r },
                        ),
                    },
                    else => Error.InterpreterError,
                };
            },
            else => {},
        }
        return Error.InterpreterError;
    }
    pub fn evaluate(self: *Interpreter, expression: *const ast.Expr) errors!Object {
        return switch (expression.*) {
            .literal => |literal| {
                switch (literal.value.?) {
                    .number => |num| return Object{ .number = num },
                    .string => |str| return Object{ .string = str },
                }
            },
            .grouping => |grouping| self.evaluate(grouping.expression),
            .unary => |unary| self.evalUnary(unary),
            .binary => |binary| self.evalBinary(binary),
        };
    }
};

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
    nil: ?void,
    boolean: bool,
    number: f64,
    string: []const u8,
};

pub const Interpreter = struct {
    arena: std.heap.ArenaAllocator,

    const Error = error{
        RuntimeError,
    };

    const Errors = Error || main.Errors;

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

    fn _error(
        self: *Interpreter,
        token: Token,
        message: []const u8,
    ) !void {
        _ = self;
        try main._error(
            .{ .token = token },
            message,
        );
    }

    fn truthVal(self: *Interpreter, object: Object) bool {
        _ = self;
        return switch (object) {
            .boolean => |boolean| return boolean,
            .nil => return false,
            else => true,
        };
    }

    fn evalUnary(self: *Interpreter, expression: ast.Unary) !Object {
        const right = try self.evaluate(expression.right);
        const operator = expression.operator._type;
        switch (operator) {
            Type.MINUS => {
                switch (right) {
                    .number => |num| return Object{ .number = -1 * num },
                    else => {
                        try self._error(
                            expression.operator,
                            "Operand must be a number.",
                        );
                        return Error.RuntimeError;
                    },
                }
            },
            Type.BANG => return Object{ .boolean = !self.truthVal(right) },
            else => {
                try self._error(
                    expression.operator,
                    "is not an unary operator.",
                );
                return Error.RuntimeError;
            },
        }
        return Error.RuntimeError;
    }
    fn evalBinary(self: *Interpreter, expression: ast.Binary) !Object {
        const left = try self.evaluate(expression.left);
        const right = try self.evaluate(expression.right);
        if (@intFromEnum(left) != @intFromEnum(right)) {
            try self._error(
                expression.operator,
                "Binary operations between different types are not supported.",
            );
            return Error.RuntimeError;
        }
        switch (left) {
            .number => {
                const l = left.number;
                const r = right.number;
                return switch (expression.operator._type) {
                    Type.PLUS => Object{ .number = l + r },
                    Type.MINUS => Object{ .number = l - r },
                    Type.STAR => Object{ .number = l * r },
                    Type.SLASH => Object{ .number = l / r },

                    // Not reversing left and right operands gives incorrect results.
                    Type.GREATER => Object{ .boolean = r < l },
                    Type.GREATER_EQUAL => Object{ .boolean = r <= l },
                    Type.LESS => Object{ .boolean = r > l },
                    Type.LESS_EQUAL => Object{ .boolean = r >= l },
                    Type.EQUAL_EQUAL => Object{ .boolean = r == l },
                    Type.BANG_EQUAL => Object{ .boolean = r != l },

                    else => {
                        try self._error(
                            expression.operator,
                            "is not an binary operator for numbers.",
                        );
                        return Error.RuntimeError;
                    },
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
                    Type.EQUAL_EQUAL => Object{ .boolean = std.mem.eql(u8, r, l) },
                    Type.BANG_EQUAL => Object{ .boolean = !std.mem.eql(u8, r, l) },
                    else => {
                        try self._error(
                            expression.operator,
                            "is not an binary operator for strings.",
                        );
                        return Error.RuntimeError;
                    },
                };
            },
            else => {
                try self._error(
                    expression.operator,
                    "Binary operations are not support for type.",
                );
                return Error.RuntimeError;
            },
        }
        return Error.RuntimeError;
    }
    fn evaluate(self: *Interpreter, expression: *const ast.Expr) Errors!Object {
        return switch (expression.*) {
            .literal => |literal| {
                if (literal.value == null) return Object{ .nil = null };
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

    fn stringify(self: *Interpreter, object: Object) ![]const u8 {
        return switch (object) {
            .nil => "null",
            .string => |str| str,
            .number => |num| try std.fmt.allocPrint(
                self.arena.allocator(),
                "{d}",
                .{num},
            ),
            .boolean => |boolean| switch (boolean) {
                true => "true",
                false => "false",
            },
        };
    }
    pub fn interpret(self: *Interpreter, expression: *const ast.Expr) !void {
        const value = try self.evaluate(expression);
        try main.stdout.print("{!s}\n", .{self.stringify(value)});
    }
};

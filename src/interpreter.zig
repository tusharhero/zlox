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

fn _error(token: Token, message: []const u8) !void {
    try main._error(
        .{ .token = token },
        message,
    );
}
const Error = error{
    RuntimeError,
};

pub const Env = struct {
    values: std.StringArrayHashMap(Object),
    enclosing_environment: ?*Env,
    arena: std.heap.ArenaAllocator,

    /// Caller must call deinit.
    pub fn init(allocator: std.mem.Allocator, enclosing_environment: ?*Env) Env {
        return Env{
            .values = std
                .StringArrayHashMap(Object).init(allocator),
            .enclosing_environment = enclosing_environment,
            .arena = std.heap.ArenaAllocator.init(
                std.heap.page_allocator,
            ),
        };
    }
    pub fn deinit(self: *Env) void {
        self.arena.deinit();
        self.values.deinit();
    }

    pub fn define(self: *Env, name: []const u8, value: Object) !void {
        // Duplicate the name because its string maybe freed.
        const name_dupe = try self.arena.allocator().dupe(u8, name);
        try self.values.put(name_dupe, value);
    }
    pub fn assign(self: *Env, name: Token, value: Object) !void {
        if (self.values.contains(name.lexeme)) {
            // Duplicate the name because its string maybe freed.
            const name_dupe = try self.arena.allocator().dupe(u8, name.lexeme);
            try self.values.put(name_dupe, value);
        } else {
            if (self.enclosing_environment != null) {
                try self.enclosing_environment.?.assign(name, value);
            } else {
                try _error(name, "Undefined variable.");
                return;
            }
        }
    }

    pub fn get(self: *Env, name: Token) !Object {
        if (self.values.contains(name.lexeme))
            return self.values.get(name.lexeme).?;
        if (self.enclosing_environment != null) {
            return self.enclosing_environment.?.get(name);
        }
        try _error(name, "Undefined variable.");

        return Error.RuntimeError;
    }

    pub fn debugPrint(self: *Env) !void {
        var it = self.values.iterator();
        var i: u64 = 0;
        while (it.next()) |pair| : (i += 1) {
            std.debug.print(
                "{d}: {s}: {any}\n",
                .{ i, pair.key_ptr.*, pair.value_ptr },
            );
        }
    }
};
pub const Interpreter = struct {
    arena: std.heap.ArenaAllocator,
    environment: Env,

    const Errors = Error || main.Errors;

    /// Caller must call deinit.
    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return Interpreter{
            .arena = std.heap.ArenaAllocator.init(
                std.heap.page_allocator,
            ),
            .environment = Env.init(allocator, null),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.arena.deinit();
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
                        try _error(
                            expression.operator,
                            "Operand must be a number.",
                        );
                        return Error.RuntimeError;
                    },
                }
            },
            Type.BANG => return Object{ .boolean = !self.truthVal(right) },
            else => {
                try _error(
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
            try _error(
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
                        try _error(
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
                        try _error(
                            expression.operator,
                            "is not an binary operator for strings.",
                        );
                        return Error.RuntimeError;
                    },
                };
            },
            else => {
                try _error(
                    expression.operator,
                    "Binary operations are not support for type.",
                );
                return Error.RuntimeError;
            },
        }
        return Error.RuntimeError;
    }

    fn evalAssignment(self: *Interpreter, expression: ast.Assignment) !Object {
        const value = try self.evaluate(expression.value);
        try self.environment.assign(expression.name, value);
        return value;
    }
    fn evaluate(self: *Interpreter, expression: *const ast.Expr) Errors!Object {
        return switch (expression.*) {
            .literal => |literal| {
                if (literal.value == null) return Object{ .nil = null };
                switch (literal.value.?) {
                    .number => |num| return Object{ .number = num },
                    .string => |str| return Object{ .string = str },
                    .boolean => |boolean| return Object{ .boolean = boolean },
                }
            },
            .grouping => |grouping| self.evaluate(grouping.expression),
            .unary => |unary| self.evalUnary(unary),
            .binary => |binary| self.evalBinary(binary),
            .variable => |variable| self.environment.get(variable.name),
            .assignment => |assignment| self.evalAssignment(assignment),
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

    fn expressionStatement(self: *Interpreter, stmt: *const ast.Stmt) !void {
        _ = try self.evaluate(stmt.expression);
    }

    fn printStatement(self: *Interpreter, stmt: *const ast.Stmt) !void {
        const value = try self.evaluate(stmt.print);
        try main.stdout.print("{!s}\n", .{self.stringify(value)});
    }
    fn varStatement(self: *Interpreter, stmt: ast.VarDecl) !void {
        var value: Object = Object{ .nil = null };
        if (stmt.intializer != null)
            value = try self.evaluate(stmt.intializer.?);
        try self.environment.define(stmt.name.lexeme, value);
    }
    fn blockStatement(self: *Interpreter, block: ast.Block) Errors!void {
        const allocator = self.arena.allocator();
        const prev = try allocator.create(Env);
        prev.* = self.environment;
        self.environment = Env.init(allocator, prev);
        for (block.statements.items) |statement| {
            try self.execute(&statement);
        }
        self.environment = prev.*;
    }

    fn ifStatement(self: *Interpreter, statement: ast.IfStmt) Errors!void {
        const truthy = self.truthVal(try self.evaluate(statement.condition));
        if (truthy)
            try self.execute(statement.thenBranch)
        else if (statement.elseBranch != null)
            try self.execute(statement.elseBranch.?);
    }

    fn execute(self: *Interpreter, statement: *const ast.Stmt) !void {
        try switch (statement.*) {
            .expression => self.expressionStatement(statement),
            .print => self.printStatement(statement),
            .variable => |_var| self.varStatement(_var),
            .block => |block| self.blockStatement(block),
            ._if => |_if| self.ifStatement(_if),
        };
    }

    pub fn interpret(self: *Interpreter, statements: std.ArrayList(*ast.Stmt)) !void {
        for (statements.items) |statement| {
            try self.execute(statement);
        }
    }
};

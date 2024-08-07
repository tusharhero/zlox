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
const Token = _tokens.Token;
const Type = _tokens.TokenType;
const ast = @import("ast.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const main = @import("main.zig");

pub fn Resolver(Writer: type) type {
    return struct {
        arena: std.heap.ArenaAllocator,
        interpreter: *Interpreter(Writer),
        scopes: Scopes,
        current_function: FunctionType,

        const Scopes = std.ArrayList(*Scope);
        const Scope = std.StringHashMap(bool);
        const FunctionType = enum { NONE, FUNCTION };
        const Self = @This();

        /// Caller must call deinit.
        pub fn init(allocator: std.mem.Allocator, interpreter: *Interpreter(Writer)) Self {
            return Self{
                .arena = std.heap.ArenaAllocator.init(allocator),
                .interpreter = interpreter,
                .scopes = Scopes.init(allocator),
                .current_function = FunctionType.NONE,
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
            self.scopes.deinit();
        }

        fn beginScope(self: *Self) !void {
            const allocator = self.arena.allocator();
            const scope = try allocator.create(Scope);
            scope.* = Scope.init(allocator);
            try self.scopes.append(scope);
        }

        fn endScope(self: *Self) void {
            _ = self.scopes.pop();
        }

        fn declare(self: *Self, name: Token) !void {
            if (self.scopes.getLastOrNull()) |scope|
                try scope.put(name.lexeme, false);
        }

        fn define(self: *Self, name: Token) !void {
            if (self.scopes.getLastOrNull()) |scope|
                try scope.put(name.lexeme, true);
        }

        pub fn resolve(self: *Self, resolvee: anytype) main.Errors!void {
            switch (@TypeOf(resolvee)) {
                *const ast.Stmt, *ast.Stmt => try self.resolveStatement(resolvee),
                *const ast.Expr => try self.resolveExpression(resolvee),
                []const *ast.Stmt => for (resolvee) |_statement|
                    try self.resolve(_statement),
                else => |invalid_type| @panic("Invalid type " ++ @typeName(invalid_type) ++ " passed to resolve."),
            }
        }

        fn resolveLocal(self: *Self, expression: *const ast.Expr, name: Token) !void {
            const size = self.scopes.items.len;
            if (size == 0) return;
            for (0..size) |i| {
                if (self.scopes.items[size - 1 - i].contains(name.lexeme)) {
                    try self.interpreter.resolve(expression, i);
                    break;
                }
            }
        }

        fn resolveStatement(self: *Self, statement: *const ast.Stmt) !void {
            switch (statement.*) {
                .block => |block| {
                    try self.beginScope();
                    for (block.statements.items) |_statement| {
                        try self.resolve(&_statement);
                    }
                    self.endScope();
                },
                .variable => |variable| {
                    try self.declare(variable.name);
                    if (variable.intializer) |initializer|
                        try self.resolve(initializer);
                    try self.define(variable.name);
                },
                .expression => |expression| try self.resolve(expression),
                .print => |print| try self.resolve(print),
                ._if => |_if| {
                    try self.resolve(_if.condition);
                    try self.resolve(_if.thenBranch);
                    if (_if.elseBranch) |elseBranch|
                        try self.resolve(elseBranch);
                },
                .function => |function| {
                    try self.declare(function.name);
                    try self.define(function.name);
                    const enclosing_function = self.current_function;
                    self.current_function = FunctionType.FUNCTION;
                    try self.beginScope();
                    if (function.parameters) |parameters|
                        for (parameters.items) |parameter| {
                            try self.declare(parameter);
                            try self.define(parameter);
                        };
                    for (function.body.items) |_statement|
                        try self.resolve(&_statement);
                    self.endScope();
                    self.current_function = enclosing_function;
                },
                .class => |class| {
                    try self.declare(class.name);
                    try self.define(class.name);
                },
                ._return => |_return| if (self.current_function != FunctionType.NONE) {
                    if (_return.value) |value|
                        try self.resolve(value);
                } else try main._error(
                    .{ .token = _return.keyword },
                    "Can't return from top-level code.",
                ),
                ._while => |_while| {
                    try self.resolve(_while.condition);
                    try self.resolve(_while.body);
                },
            }
        }

        fn resolveExpression(self: *Self, expression: *const ast.Expr) !void {
            try switch (expression.*) {
                .variable => |variable| {
                    if (self.scopes.getLastOrNull()) |scope|
                        if (scope.get(variable.name.lexeme)) |defined|
                            if (!defined)
                                try main._error(
                                    .{ .token = variable.name },
                                    "Can't read local variable in its own initializer.",
                                );
                    try self.resolveLocal(expression, variable.name);
                },
                .unary => |unary| self.resolve(unary.right),
                .binary, .logical => |binary| {
                    try self.resolve(binary.left);
                    try self.resolve(binary.right);
                },
                .literal => {},
                .assignment => |assignment| {
                    try self.resolve(assignment.value);
                    try self.resolveLocal(expression, assignment.name);
                },
                .call => |call| {
                    try self.resolve(call.callee);
                    if (call.arguments) |arguments|
                        for (arguments.items) |argument|
                            try self.resolve(argument);
                },
                .get => |get| {
                    try self.resolve(get.object);
                },
                .set => |set| {
                    try self.resolve(set.value);
                    try self.resolve(set.object);
                },
                .grouping => |grouping| try self.resolve(grouping.expression),
            };
        }
    };
}
